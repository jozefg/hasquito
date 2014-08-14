module Language.Hasquito.Closure where
import Control.Applicative
import Control.Monad.Writer
import Data.List
import Language.Hasquito.Syntax
import Language.Hasquito.Util

freeVars :: Exp -> [Name]
freeVars Num{}            = []
freeVars Op{}             = []
freeVars (Var n)          = [n]
freeVars (App l r)        = freeVars l ++ freeVars r
freeVars (Lam _ var body) = freeVars body \\ [var]

saturate :: Exp -> CompilerM Exp
saturate (Op o) = do
  [l, r] <- sequence [freshName, freshName]
  return $ Lam [] l $ Lam [] r (App (App (Op o) (Var l)) $ Var r)
saturate (App l r) = App <$> saturate l <*> saturate r
saturate (Lam cs vs body) = Lam cs vs <$> saturate body
saturate e = return e

closConv :: Exp -> Exp
closConv (Num i) = Num i
closConv (Op p) = Op p
closConv (Var n) = Var n
closConv (App l r) = closConv l `App` closConv r
closConv (Lam _ vars body) =
  let others = freeVars (Lam [] vars body)
  in Lam others vars (closConv body)

liftLam :: Exp -> WriterT [Def] CompilerM Exp
liftLam (Num i) = return $ Num i
liftLam (Op p) = return $ Op p
liftLam (Var n) = return $ Var n
liftLam (App l r) = App <$> liftLam l <*> liftLam r
liftLam l@(Lam closed var body) = do
  body' <- liftLam body
  name <- lift freshName
  tell [Def TNum name (Lam closed var body')]
  return (Var name)

simplify :: [Def] -> CompilerM [Def]
simplify = fmap concat . mapM scify
  where scify d = do
          b <- saturate (defBody d)
          (b', lifts) <- runWriterT $ liftLam (closConv b)
          return $ d{defBody = b'} : lifts
