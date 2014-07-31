module Language.Hasquito.Closure where
import Control.Applicative
import Control.Monad.Writer
import Data.List
import Language.Hasquito.Syntax
import Language.Hasquito.Util

freeVars :: Exp -> [Name]
freeVars Num{}           = []
freeVars Op{}            = []
freeVars (Var n)         = [n]
freeVars (App l r)       = freeVars l ++ freeVars r
freeVars (Lam closed vars body) = freeVars body \\ (vars ++ closed)

closConv :: Exp -> Exp
closConv (Num i) = Num i
closConv (Op p) = Op p
closConv (Var n) = Var n
closConv (App l r) = closConv l `App` closConv r
closConv (Lam [] vars body) =
  let others = freeVars (Lam [] vars body)
  in Lam others vars body

liftLam :: Exp -> WriterT [Def ()] CompilerM Exp
liftLam (Num i) = return $ Num i
liftLam (Op p) = return $ Op p
liftLam (Var n) = return $ Var n
liftLam (App l r) = App <$> liftLam l <*> liftLam r
liftLam l@(Lam{}) = do
  name <- lift freshName
  tell [Def TNum name l ()]
  return (Var name)

removeClos :: [Def ()] -> CompilerM [Def ()]
removeClos = fmap concat . mapM scify
  where scify d@Def{defBody = b} = do
          (b', lifts) <- runWriterT $ liftLam (closConv b)
          return $ d{defBody = b'} : lifts
