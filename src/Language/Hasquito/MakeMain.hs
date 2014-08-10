{-# LANGUAGE OverloadedStrings #-}
module Language.Hasquito.MakeMain where
import Control.Applicative
import Language.Hasquito.JSify
import Language.Hasquito.Util
import Language.JavaScript.AST
import Language.JavaScript.NonEmptyList
import Control.Monad.Except
import Data.Function (on)


mainCall :: CompilerM Stmt
mainCall = do
  enterName <- either err return $ name "enterMain"
  mainName  <- either err return $ name "main"
  return . StmtExpr $
    singleton (LValue enterName []) `ESApply`
     (RVInvoke . singleton . Invocation) [ExprName mainName]
  where err _ = throwError . Impossible $ "Cannot convert name to js var"

makeMain :: [VarDecl] -> CompilerM Program
makeMain vars = Program <$> varStmt vars <*> fmap (:[]) mainCall
  where varStmt (v : vs) = return . (:[]) . VarStmt $ foldr (<:>) (singleton v) vs
        varStmt _        = throwError . Impossible $ "Empty program!"

