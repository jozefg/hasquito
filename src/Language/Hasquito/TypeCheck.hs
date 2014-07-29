{-# LANGUAGE LambdaCase, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Hasquito.TypeCheck where
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Text as T
import           Language.Hasquito.Syntax

-- | The type of type constraints
data Constr = Ty :~: Ty

-- | Substitution for type variables
type Subst = M.Map Name Ty

-- | The type checker monad
type TCM = ReaderT (M.Map Name Ty) CompilerM

substTy :: Name -> Ty -> Ty -> Ty
substTy _ _ TNum = TNum
substTy n e (TArr l r) = substTy n e l `TArr` substTy n e r
substTy n e (TVar m) | n == m     = e
                     | otherwise = TVar m

subst :: Name -> Ty -> [Constr] -> [Constr]
subst n ty = map $ \(a :~: b) -> substTy n ty a :~: substTy n ty b

-- | Unify solves a list of constraints and produces
-- the corresponding type substitution. It potentially
-- throws errors if things won't unify.
unify :: [Constr] -> TCM Subst
unify [] = return $ M.empty
unify ((TNum :~: TNum) : rest) = unify rest
unify ((TArr l r :~: TArr l' r') : rest) = unify (l :~: l' : r :~: r' : rest)
unify ((TVar n :~: e) : rest) = M.insert n e <$> unify (subst n e rest)
unify ((e :~: TVar n) : rest) = M.insert n e <$> unify (subst n e rest)
unify ((l :~: r) : _) = throwError . TCError $
                        "Couldn't unify " <> T.pack (show l) <> " with " <> T.pack (show r)

useSubst :: Ty -> Subst -> Ty
useSubst ty = M.foldWithKey substTy ty

lookupVar :: (MonadReader (M.Map Name Ty) m, MonadError Error m) => Name -> m Ty
lookupVar v = asks (M.lookup v) >>= \case
  Nothing -> throwError . TCError $ "No such variable " <> T.pack (show v)
  Just ty -> return ty

typeOf :: Exp -> WriterT [Constr] TCM Ty
typeOf Num {} = return TNum
typeOf Prim{} = return $ TNum `TArr` TNum `TArr` TNum
typeOf (Var v) = lookupVar v
typeOf (Lam vars body) = do
  resultTy <- local (M.union $ M.fromList vars) $ typeOf body
  return . foldr TArr resultTy . map snd $ vars
typeOf (App l r) = do
  funTy <- typeOf l
  argTy <- typeOf r
  [lvar, rvar] <- mapM (lift . lift) [freshName, freshName]
  tell [TVar lvar `TArr` TVar rvar :~: funTy, TVar lvar :~: argTy]
  return (TVar rvar)

typeGlobal :: (M.Map Name Ty) -> Def m -> CompilerM (Def m)
typeGlobal globals d@Def{..} = flip runReaderT globals $ do
  (ty, constr) <- runWriterT $ typeOf defBody
  _ <- unify $ ty :~: defTy : constr  -- Should sanity check result...
  return d

typeCheck :: [Def m] -> CompilerM [Def m]
typeCheck defs = let globals = M.fromList $ zip (map defName defs) (map defTy defs)
                 in mapM (typeGlobal globals) defs
