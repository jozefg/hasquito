module Language.Hasquito.Closure where
import Data.List
import Language.Hasquito.Syntax

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
