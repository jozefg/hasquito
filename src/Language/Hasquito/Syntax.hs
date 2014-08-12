module Language.Hasquito.Syntax where
import qualified Data.Text as T

data Name = Gen Int | Name T.Text
          deriving(Eq, Show, Ord)
instance Enum Name where
  toEnum           = Gen
  fromEnum (Gen i) = i
  fromEnum Name{}  = error "Impossible! Attempted to fromEnum a Name"

data Flex = Flexible | Rigid
          deriving (Eq, Show)

-- | Type of all Hasquito expression
data Ty = TArr Ty Ty         -- ^ Function type
        | TNum               -- ^ Numeric type
        | TVar (Maybe Name) Flex Name -- ^ type var, annotated with it's flexibility and maybe it's scope
        deriving (Eq, Show)

data Op = Plus | Minus | Mult | Div
          deriving(Eq, Show)

data Exp = Op Op
         | Num Int
         | Var Name
         | App Exp Exp -- Curried application
         | Lam [Name] Name Exp
         deriving(Eq, Show)

data Def = Def { defTy    :: Ty
               , defName  :: Name
               , defBody  :: Exp }
              deriving(Eq, Show)
