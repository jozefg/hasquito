{-# LANGUAGE OverloadedStrings #-}
module Language.Hasquito.MakeMain where
import Control.Applicative
import Language.Hasquito.Util
import Language.JavaScript.AST
import Language.JavaScript.NonEmptyList
import Control.Monad.Except


mainCall :: CompilerM Stmt
mainCall = do
  enterName <- either err return $ name "enterMain"
  return . StmtExpr $
    singleton (LValue enterName []) `ESApply`
     (RVInvoke . singleton . Invocation) []
  where err _ = throwError . Impossible $ "Cannot convert name to js var"

makeMain :: [VarDecl] -> CompilerM Program
makeMain vars = Program (VarStmt . singleton <$> vars) <$> fmap (:[]) mainCall

