{-# LANGUAGE RecordWildCards #-}
module Language.Hasquito.TypeCheck where
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map as M
import           Data.Maybe
import           Language.Hasquito.Syntax


-- | The type of type constraints
data Constr = Ty :~: Ty

-- | Substitution for type variables
type Subst = M.Map Name Ty

-- | The type checker monad
type TCM = ReaderT (M.Map Name Ty) CompilerM

subst :: Name -> Ty -> [Constr] -> [Constr]
subst n ty = map $ \(a :~: b) -> change n ty a :~: change n ty b
  where change _ _ TNum = TNum
        change n e (TArr l r) = change n e l `TArr` change n e r
        change n e (TVar m) | n == m     = e
                            | otherwise = TVar m

-- | Unify solves a list of constraints and produces
-- the corresponding type substitution. It potentially
-- throws errors if things won't unify.
unify :: [Constr] -> TCM Subst
unify [] = return $ M.empty
unify ((TNum :~: TNum) : rest) = unify rest
unify ((TArr l r :~: TArr l' r') : rest) = unify (l :~: l' : r :~: r' : rest)
unify ((TVar n :~: e) : rest) = M.insert n e <$> unify (subst n e rest)
unify ((e :~: TVar n) : rest) = M.insert n e <$> unify (subst n e rest)
unify ((l :~: r) : _) = throwError . TCError $ "Couldn't unify " ++ show l ++ " with " ++ show r

useSubst :: Name -> Subst -> Ty
useSubst n = fromMaybe (TVar n) . M.lookup n

typeOf :: Exp -> TCM Ty
typeOf Num {} = return TNum
typeOf Prim{} = return $ TNum `TArr` TNum `TArr` TNum
typeOf (Var v) = do
  tyinfo <- asks (M.lookup v)
  case tyinfo of
    Just ty -> return ty
    Nothing -> throwError . TCError $ "Unbound variable " ++ show v
typeOf (Lam vars body) = do
  resultTy <- local (M.union $ M.fromList vars) $ typeOf body
  let argTys = map snd vars
  return $ foldr TArr resultTy argTys
typeOf (App l r) = do
  funTy <- typeOf l
  argTy <- typeOf r
  [lvar, rvar] <- mapM lift [freshName, freshName]
  sub <- unify [TVar lvar `TArr` TVar rvar :~: funTy, TVar lvar :~: argTy]
  return $ useSubst rvar sub

typeCheck :: Def m -> CompilerM (Def m)
typeCheck d@Def{..} = flip runReaderT M.empty $
                      case defTy of
                        Just ty -> do
                          ty' <- typeOf defBody
                          unify [ty :~: ty']
                          return d
                        Nothing -> do
                          ty <- typeOf defBody
                          return d{defTy = Just ty}
