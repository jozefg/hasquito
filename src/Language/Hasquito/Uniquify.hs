{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Language.Hasquito.Uniquify where
import           Control.Applicative
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

uniqueSTG :: Name -> SExp -> UniquifyM SExp
uniqueSTG _ (SVar v) = asks (M.lookup v) >>= \case
  Just v' -> return (SVar v') -- Local, mangled variable
  Nothing -> return (SVar v)  -- Global, already unique variable
uniqueSTG _ (SNum i) = return $ SNum i
uniqueSTG n (SApp l r) = SApp <$> uniqueSTG n l <*> uniqueSTG n r
uniqueSTG n (FullApp o l r) = return $ FullApp o (mangle n l) (mangle n r)

uniquify :: [TopLevel] -> CompilerM [TopLevel]
uniquify = mapM unique
  where unique (Thunk closed name body) =
          let closed' = map (mangle name) closed
              mangleMap = M.empty -- M.fromList $ zip closed closed'
          in Thunk closed name <$> runReaderT (uniqueSTG name body) mangleMap
        unique (Fun name closed arg body) =
          let closed'   = map (mangle name) closed
              arg'      = mangle name arg
              mangleMap = M.empty -- (arg, arg') : zip closed closed'
          in Fun name closed arg <$> runReaderT (uniqueSTG name body) mangleMap
          

