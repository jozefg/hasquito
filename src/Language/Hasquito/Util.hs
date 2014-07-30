{-# LANGUAGE OverloadedStrings #-}
module Language.Hasquito.Util where
import           Control.Monad.Except
import           Control.Monad.Gen
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import           Language.Hasquito.Syntax

-- | Errors from some part of the application
data Error = TCError T.Text
           | ParseError T.Text
           deriving Show

type CompilerM = ExceptT Error (Gen Name) 

freshName :: CompilerM Name -- I really need to update monad-gen
freshName = lift gen

class Build a where
  build :: a -> B.Builder

instance Build Name where
  build (Name s) = B.fromText s
  build (Gen i)  = B.fromString (show i)

instance Build Ty where
  build TNum       = B.fromString "Num"
  build (TArr l r) = "(" <> build l <> ") -> (" <> build r <> ")"
  build (TVar v)   = build v

instance Build Prim where
  build Plus  = B.singleton '+'
  build Mult  = B.singleton '*'
  build Minus = B.singleton '-'
  build Div   = B.singleton '/'

buildBinding :: (Name, Ty) -> B.Builder
buildBinding (v, ty) = "(" <> build v <> " : " <> build ty <> ")"

instance Build Exp where
  build (Num i)   = B.fromString (show i)
  build (App l r) = "(" <> build l <> ") (" <> build r <> ")"
  build (Prim p)  = build p
  build (Var v)   = build v
  build (Lam vars body) =
    let bindings = foldr (<>) mempty . map buildBinding $ vars
    in "(\\" <> bindings <> " -> " <> build body <> ")"

instance Build (Def m) where
  build (Def ty nm body _) = build nm <> " : " <> build ty <> " = " <> build body

pretty :: Build a => a -> T.Text
pretty = L.toStrict . B.toLazyText . build
