module Language.Hasquito.JSify where
import           Control.Applicative
import           Control.Monad.Except
import qualified Data.Text as T
import           Language.Hasquito.STG
import qualified Language.Hasquito.Syntax as S
import           Language.Hasquito.Util
import           Language.JavaScript.AST
import           Language.JavaScript.NonEmptyList

jname :: String -> CompilerM Name
jname = either (throwError . Impossible . T.pack) return . name

jvar :: S.Name -> CompilerM Name
jvar (S.Gen i)  = either (throwError . Impossible . T.pack) return
                  . name
                  $ ('_' : show i)
jvar (S.Name s) = either (throwError . Impossible . T.pack) return
                  . name
                  . T.unpack
                  $ s

block :: [CompilerM Stmt] -> CompilerM Stmt
block = fmap dummyIf . sequence
  where dummyIf ss = StmtIf $ IfStmt (ExprLit $ LitBool True) ss Nothing

opCont :: S.Op -> CompilerM Name
opCont S.Plus  = jname "doPlus"
opCont S.Minus = jname "doMinus"
opCont S.Mult  = jname "doMult"
opCont S.Div   = jname "doDiv"

enter :: Expr -> CompilerM Stmt
enter = undefined

-- This is a fun word.
closurify :: Expr -> CompilerM Expr
closurify = undefined

pushStack :: Expr -> Name -> CompilerM Stmt
pushStack exp nm = do
  closed   <- closurify exp
  push     <- jname "push"
  return . StmtExpr $
    singleton (LValue nm [([], Property push)]) `ESApply`
    (RVInvoke . singleton . Invocation) [exp]
  
pushArg :: Expr -> CompilerM Stmt
pushArg e = jname "ARG_STACK" >>= pushStack e

pushCont :: Expr -> CompilerM Stmt
pushCont e = jname "CONT_STACK" >>= pushStack e

pushEval :: Expr -> CompilerM Stmt
pushEval e = jname "EVAL_STACK" >>= pushStack e

eval :: CompilerM Expr
eval = ExprName <$> jname "evalFirst"

jump :: CompilerM Stmt
jump = do
  jump <- jname "jumpNext"
  return . StmtExpr $
    singleton (LValue jump []) `ESApply`
     (RVInvoke . singleton . Invocation) []

prim :: S.Op -> Expr -> Expr -> CompilerM Stmt
prim op l r = block [ pushArg r
                    , opCont op >>= pushCont . ExprName
                    , eval >>= pushCont
                    , eval >>= pushCont
                    , enter l ]


lit :: Int -> CompilerM Stmt
lit i = block [ pushEval . ExprLit . LitNumber . Number . fromIntegral $ i
              , jump ]
