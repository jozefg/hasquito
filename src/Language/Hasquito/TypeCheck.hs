module Language.Hasquito.TypeCheck where
import qualified Data.Map as M
import Language.Hasquito.Syntax

data Constr a = a :~: a
type Subst = M.Map Name Ty
