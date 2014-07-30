module Language.Hasquito.Closure where
import Control.Applicative
import Control.Monad.Writer
import Data.List
import Language.Hasquito.Syntax
import Language.Hasquito.Util

freeVars :: Exp -> [Name]
freeVars Num{}           = []
freeVars Prim{}          = []
freeVars (Var n)         = [n]
freeVars (App l r)       = freeVars l ++ freeVars r
freeVars (Lam vars body) = freeVars body \\ vars

closConv :: Exp -> Exp
closConv (Num i) = Num i
closConv (Prim p) = Prim p
closConv (Var n) = Var n
closConv (App l r) = closConv l `App` closConv r
closConv (Lam vars body) =
  let others = freeVars (Lam vars body)
  in Lam (others ++ vars) body

liftLam :: Exp -> WriterT [Def ()] CompilerM Exp
liftLam (Num i) = return $ Num i
liftLam (Prim p) = return $ Prim p
liftLam (Var n) = return $ Var n
liftLam (App l r) = App <$> liftLam l <*> liftLam r
liftLam l@(Lam vars body) = do
  name <- lift freshName
  tell [Def TNum name l ()]
  return (Var name)

removeClos :: [Def ()] -> CompilerM [Def ()]
removeClos = fmap concat . mapM scify
  where scify d@Def{defBody = b} = do
          (b', lifts) <- runWriterT $ liftLam (closConv b)
          return $ d{defBody = b'} : lifts
