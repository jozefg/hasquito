{-# LANGUAGE OverloadedStrings #-}
module Language.Hasquito.JSify where
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Text as T
import           Language.Hasquito.STG
import qualified Language.Hasquito.Syntax as S
import           Language.Hasquito.Util
import           Language.JavaScript.AST
import           Language.JavaScript.NonEmptyList

data Closure = Closure { topClos  :: M.Map S.Name [S.Name] -- ^ A map of names to closed variables
                       , currClos :: M.Map S.Name Int      -- ^ A map of closed over variables to their current position
                       }
type CodeGenM = ReaderT Closure CompilerM

jname :: String -> CodeGenM Name
jname = either (throwError . Impossible . T.pack) return . name

jvar :: S.Name -> CodeGenM Name
jvar (S.Gen i)  = jname ('_' : show i)
jvar (S.Name s) = jname . T.unpack $ s

opCont :: S.Op -> CodeGenM Name
opCont S.Plus  = jname "doPlus"
opCont S.Minus = jname "doMinus"
opCont S.Mult  = jname "doMult"
opCont S.Div   = jname "doDiv"

enter :: Expr -> CodeGenM Stmt
enter e = do
  enterName <- jname "enter"
  return . StmtExpr $
    singleton (LValue enterName []) `ESApply`
     (RVInvoke . singleton . Invocation) [e]

index :: Int -> CodeGenM Expr
index i = do
  [node, closed] <- sequence [jname "NODE", jname "closed_vars"]
  return $
    ExprRefinement (ExprName node) (Property closed)
    `ExprRefinement` Subscript (ExprLit . LitNumber . Number $ fromIntegral i)

mkClosure :: Expr -> [Expr] -> CodeGenM Expr
mkClosure f args = do
  mk <- ExprName <$> jname "mkClosure"
  return $ ExprInvocation mk (Invocation $ [f, list])
  where list = ExprLit . LitArray . ArrayLit $ args

findVar :: M.Map S.Name Int -> S.Name -> CodeGenM Expr
findVar m name = case M.lookup name m of
  Just i  -> index i
  Nothing -> ExprName <$> jvar name

resolve :: S.Name -> CodeGenM Expr -> CodeGenM Expr
resolve nm expr = do
  result <- asks (M.lookup nm . topClos)
  case result of
    Nothing -> expr
    Just cs -> do
      clos <- asks currClos
      closure <- mapM (findVar clos) cs
      expr >>= flip mkClosure closure

pushStack :: Expr -> Name -> CodeGenM Stmt
pushStack exp nm = do
  push     <- jname "push"
  return . StmtExpr $
    singleton (LValue nm [([], Property push)]) `ESApply`
    (RVInvoke . singleton . Invocation) [exp]
  
pushArg :: Expr -> CodeGenM Stmt
pushArg e = jname "ARG_STACK" >>= pushStack e

pushCont :: Expr -> CodeGenM Stmt
pushCont e = jname "CONT_STACK" >>= pushStack e

pushEval :: Expr -> CodeGenM Stmt
pushEval e = jname "EVAL_STACK" >>= pushStack e

eval :: CodeGenM Expr
eval = ExprName <$> jname "evalFirst"

sif :: Expr -> Expr -> Expr -> CodeGenM Stmt
sif n l r = do
  sif <- jname "sif"
  return . StmtExpr $
    singleton (LValue sif []) `ESApply`
     (RVInvoke . singleton . Invocation) [n, l, r]


jump :: CodeGenM Stmt
jump = do
  jump <- jname "jumpNext"
  return . StmtExpr $
    singleton (LValue jump []) `ESApply`
     (RVInvoke . singleton . Invocation) []

nextArg :: CodeGenM Expr
nextArg = do
  next <- jname "nextArg"
  return $ ExprInvocation (ExprName next) (Invocation [])

prim :: S.Op -> Expr -> Expr -> CodeGenM [Stmt]
prim op l r = sequence [ pushArg r
                       , ExprName <$> opCont op >>= pushCont
                       , eval >>= pushCont
                       , enter l]

lit :: Int -> CodeGenM [Stmt]
lit i = sequence [ pushEval . ExprLit . LitNumber . Number . fromIntegral $ i
                 , jump ]

app :: Expr -> Expr -> CodeGenM [Stmt]
app f a = sequence [ pushArg a
                   , enter f ]

preamble :: [S.Name] -> [S.Name] -> [Stmt] -> CodeGenM FnLit
preamble bound closured body = fmap (FnLit Nothing []) $ do
  vars <- (++) <$> mapM bindArgVar bound
          <*> mapM bindClosVar (zip [0..] closured)
  return $ FnBody vars body
  where bindArgVar v       = var <$> jvar v <*> nextArg
        bindClosVar (i, v) = var <$> jvar v <*> index i
        var l r = VarStmt . singleton $ VarDecl l (Just r)

handleVar :: [S.Name] -> S.Name -> CodeGenM Expr
handleVar closed v | v `elem` closed = ExprName <$> jvar v
                   | otherwise       = resolve v (ExprName <$> jvar v)

entryCode :: [S.Name] -> SExp -> CodeGenM [Stmt]
entryCode _ (SNum i) = lit i
entryCode closed (SVar v) = (:[]) <$> (handleVar closed v >>= enter)
entryCode closed (SApp (SVar r) (SVar l)) = join $ app <$> handleVar closed r <*> handleVar closed l
entryCode closed (FullApp op l r) = join $ prim op <$> handleVar closed l <*> handleVar closed r
entryCode closed (SIf (SVar n) (SVar l) (SVar r)) = fmap (:[]). join $
                                                    sif <$> handleVar closed n
                                                        <*> handleVar closed l
                                                        <*> handleVar closed r
entryCode _ _ = throwError . Impossible $ "Found unflattened expression in entryCode generation!"

extractClosure :: TopLevel -> [S.Name]
extractClosure (Thunk closed _ _) = closed
extractClosure (Fun _ closed _ _) = closed

extractName :: TopLevel -> S.Name
extractName (Thunk _ n _) = n
extractName (Fun n _ _ _) = n

define :: Name -> FnLit -> VarDecl
define name = VarDecl name . Just . ExprLit . LitFn

jsify :: [TopLevel] -> CompilerM [VarDecl]
jsify decls = mapM compile decls
  where closureMap = M.fromList $ zip (map extractName decls) (map extractClosure decls)
        buildState = Closure closureMap . M.fromList . flip zip [0..]
        compile (Thunk closed name body) = flip runReaderT (buildState closed) $ do
          name' <- jvar name
          body' <- entryCode closed body
          fmap (define name') . preamble [] closed $ body'
        compile (Fun name closed arg body) = flip runReaderT (buildState closed) $ do
          name' <- jvar name
          body' <- entryCode (arg : closed) body
          fmap (define name') . preamble [arg] closed $ body'
