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
                       , currClos :: M.Map S.Name Int -- ^ A map of closed over variables to their current position
                       }

type CodeGenM = ReaderT Closure CompilerM

jname :: String -> CodeGenM Name
jname = either (throwError . Impossible . T.pack) return . name

jvar :: S.Name -> CodeGenM Name
jvar (S.Gen i)  = jname ('_' : show i)
jvar (S.Name s) = jname . T.unpack $ s

block :: [CodeGenM Stmt] -> CodeGenM Stmt
block = fmap dummyIf . sequence
  where dummyIf ss = StmtIf $ IfStmt (ExprLit $ LitBool True) ss Nothing

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
  return $ ExprInvocation mk (Invocation $ f:args)

resolve :: S.Name -> CodeGenM Expr -> CodeGenM Expr
resolve nm expr = do
  result <- asks (M.lookup nm . topClos)
  case result of
    Nothing -> expr
    Just cs -> do
      is <- flip map cs . (M.!) <$> asks currClos
      closure <- mapM index is
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

prim :: S.Op -> S.Name -> S.Name -> CodeGenM Stmt
prim op l r = block [ resolve r (ExprName <$> jvar r) >>= pushArg
                    , ExprName <$> opCont op          >>= pushCont
                    , eval                            >>= pushCont
                    , eval                            >>= pushCont
                    , resolve l (ExprName <$> jvar l) >>= enter]

lit :: Int -> CodeGenM Stmt
lit i = block [ pushEval . ExprLit . LitNumber . Number . fromIntegral $ i
              , jump ]

app :: Expr -> Expr -> CodeGenM Stmt
app f a = block [ pushArg a
                , enter f ]

preamble :: [S.Name] -> [S.Name] -> CodeGenM Stmt -> CodeGenM FnLit
preamble bound closured body = fmap (FnLit Nothing []) $ do
  vars <- (++) <$> mapM bindArgVar  bound
          <*> mapM bindClosVar (zip [0..] closured)
  FnBody vars . (:[]) <$> body
  where bindArgVar v       = var <$> jvar v <*> resolve v nextArg
        bindClosVar (i, v) = var <$> jvar v <*> resolve v (index i)
        var l r = VarStmt . singleton $ VarDecl l (Just r)
