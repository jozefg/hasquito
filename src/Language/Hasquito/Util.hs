{-# LANGUAGE FlexibleContexts #-}
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
           | DuplicateDefs Name
           | NoMain
           | Impossible T.Text
           deriving Show

type CompilerM = ExceptT Error (Gen Name) 

runCompilerM :: CompilerM a -> Either Error a
runCompilerM = runGen . runExceptT

freshName :: MonadGen Name m => m Name
freshName = gen

class Build a where
  build :: a -> B.Builder

instance Build Name where
  build (Name s) = B.fromText s
  build (Gen i)  = B.singleton '_' <> B.fromString (show i)

instance Build Ty where
  build TNum       = B.fromString "Num"
  build (TArr l r) = "(" <> build l <> ") -> (" <> build r <> ")"
  build (TVar _ _ v) = build v

instance Build Op where
  build Plus  = B.singleton '+'
  build Mult  = B.singleton '*'
  build Minus = B.singleton '-'
  build Div   = B.singleton '/'

instance Build Exp where
  build (Num i)   = B.fromString (show i)
  build (App l r) = "(" <> build l <> ") (" <> build r <> ")"
  build (Op p)  = build p
  build (Var v)   = build v
  build (Lam closed var body) =
    let closure =  "{" <> mconcat (map build closed) <> "}"
    in "(fun " <> closure <> " " <> build var <> " -> " <> build body <> ")"

instance Build Def where
  build (Def ty nm body) = build nm <> " : " <> build ty <> " = " <> build body

pretty :: Build a => a -> T.Text
pretty = L.toStrict . B.toLazyText . build
