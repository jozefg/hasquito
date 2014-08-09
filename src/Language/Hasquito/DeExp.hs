module DeExp where
import Control.Monad.Writer
import Language.Hasquito.STG
import Language.Hasquito.Syntax
import Language.Hasquito.Util
import Data.List ((\\))

type DeExpM = WriterT [TopLevel] CompilerM

freeVars :: SExp -> [Name]
freeVars (SVar v) = [v]
freeVars (SNum _) = []
freeVars (FullApp _ l r) = [l, r]
freeVars (SApp l r) = freeVars l ++ freeVars r


flatten :: [Name] -> SExp -> DeExpM SExp
flatten _ (SNum i) = return (SNum i)
flatten _ (SVar v) = return $ SVar v
flatten _ (FullApp op l r) = return $ FullApp op l r
flatten closed (SApp l r) = do
  [lVar, rVar] <- sequence [freshName, freshName]
  tell $ [Thunk closed lVar l
         ,Thunk closed rVar r]
  return (SApp (SVar lVar) (SVar rVar))

deExp :: [TopLevel] -> CompilerM [TopLevel]
deExp = fmap concat . mapM flattenTop
  where flattenTop (Thunk closed name body) = do
          (body', stmts) <- runWriterT (flatten closed body)
          return $ Thunk closed name body' : stmts
        flattenTop (Fun name closed arg body) = do
          (body', stmts) <- runWriterT (flatten (arg : closed) body)
          return $ Fun name closed arg body' : stmts
