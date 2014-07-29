module Language.Hasquito.Syntax where
import           Control.Monad.Except
import           Control.Monad.Gen
import qualified Data.Text as T

data Name = Gen Int | Name T.Text
          deriving(Eq, Show, Ord)
instance Enum Name where
  toEnum           = Gen
  fromEnum (Gen i) = i
  fromEnum Name{}  = error "Impossible! Attempted to fromEnum a Name"

-- | Type of all Hasquito expression
data Ty = TArr Ty Ty -- ^ Function type
        | TNum       -- ^ Numeric type
        | TVar Name  -- ^ Implicitly universally quantified type var
        deriving (Eq, Show)

data Prim = Plus | Minus | Mult | Div
          deriving(Eq, Show)

data Exp = Prim Prim
         | Num Int
         | Var Name
         | App Exp Exp -- Curried application
         | Lam [(Name, Ty)] Exp
         deriving(Eq, Show)

data Def meta = Def { defTy    :: Ty
                    , defName  :: Name
                    , defBody  :: Exp
                    , defMeta  :: meta }
              deriving(Eq, Show)
