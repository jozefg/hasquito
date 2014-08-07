{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Uniquify where
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Text as T
import           Language.Hasquito.STG
import           Language.Hasquito.Syntax
import           Language.Hasquito.Util

type UniquifyM = ReaderT (M.Map Name Name) CompilerM

-- | mangle takes a name and the scope it's in and produces a fresh
-- mangled name.
mangle :: Name -> Name -> Name
mangle scope n = Name $ pretty scope <> T.singleton '_' <> pretty n

uniqueSTG :: SExp -> UniquifyM SExp
uniqueSTG (SVar v) = asks (M.lookup v) >>= \case
  Just v' -> return (SVar v') -- Local, mangled variable
  Nothing -> return (SVar v)  -- Global, already unique variable
uniqueSTG (SNum i) = return $ SNum i
uniqueSTG (SApp l r) = SApp <$> uniqueSTG l <*> uniqueSTG r
uniqueSTG (FullApp o l r) = return $ FullApp o l r

uniquify :: [TopLevel] -> CompilerM [TopLevel]
uniquify = mapM unique
  where unique (Thunk name body) = return (Thunk name body) -- No arguments to mangle
        unique (Fun name closed arg body) =
          let closed'   = map (mangle name) closed
              arg'      = mangle name arg
              mangleMap = M.fromList $ (arg, arg') : zip closed closed'
          in Fun name closed' arg' <$> runReaderT (uniqueSTG body) mangleMap
          

