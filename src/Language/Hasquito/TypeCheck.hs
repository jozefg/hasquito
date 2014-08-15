{-# LANGUAGE LambdaCase, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Hasquito.TypeCheck where
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Map as M
import           Language.Hasquito.Syntax
import           Language.Hasquito.Util

-- | The type of type constraints
data Constr = Ty :~: Ty

-- | Substitution for type variables
type Subst = M.Map Name Ty

-- | The type checker monad
type TCM = ReaderT (M.Map Name Ty) CompilerM

-- Annotate all rigid tvars with a name
annot :: Name -> Ty -> Ty
annot n (TArr l r) = annot n l `TArr` annot n r
annot n (TVar Nothing Rigid m) = TVar (Just n) Rigid m
annot _ t = t

substTy :: Flex -> Maybe Name -> Name -> Ty -> Ty -> Ty
substTy _ _ _ _ TNum = TNum
substTy f s n e (TArr l r) = substTy f s n e l `TArr` substTy f s n e r
substTy f s n e (TVar s' f' m) | f == f' && s == s' && n == m = e
                               | otherwise = TVar s' f' m

subst :: Name -> Ty -> [Constr] -> [Constr]
subst n ty = map $ \(a :~: b) -> substTy Flexible Nothing n ty a :~: substTy Flexible Nothing n ty b

-- | Unify solves a list of constraints and produces
-- the corresponding type substitution. It potentially
-- throws errors if things won't unify.
unify :: [Constr] -> TCM Subst
unify [] = return M.empty
unify ((TNum :~: TNum) : rest) = unify rest
unify ((TArr l r :~: TArr l' r') : rest) = unify (l :~: l' : r :~: r' : rest)
unify ((TVar _ Flexible n :~: e) : rest) = M.insert n e <$> unify (subst n e rest)
unify ((e :~: TVar _ Flexible n) : rest) = M.insert n e <$> unify (subst n e rest)
unify ((l@(TVar _ Rigid _) :~: r@(TVar _ Rigid _)):rest) | l == r = unify rest -- Only unify identical rigid vars
unify ((l :~: r) : _) = throwError . TCError $
                        "Couldn't unify " <> pretty l <> " with " <> pretty r

useSubst :: Ty -> Subst -> Ty
useSubst = M.foldWithKey (substTy Flexible Nothing)

-- | This generates a "fresh" type for all global variables to
-- allow for types to specify polymorphic types.
cleanUpTVar :: Ty -> WriterT [Constr] TCM Ty
cleanUpTVar ty = do
  let rigids = rigidTVars ty
  newVars <- sequence
             . zipWith (fmap . (,)) rigids
             . repeat $ TVar Nothing Flexible <$> freshName
  return $ foldr removeRigid ty newVars
  where removeRigid ((scope, name), var) = substTy Rigid scope name var
        rigidTVars (TVar scope Rigid name) = [(scope, name)]
        rigidTVars (TArr l r)              = rigidTVars l ++ rigidTVars r
        rigidTVars _                       = []

lookupVar :: Name -> WriterT [Constr] TCM Ty
lookupVar v = asks (M.lookup v) >>= \case
  Nothing -> throwError . TCError $ "No such variable " <> pretty v
  Just ty -> cleanUpTVar ty

typeLam :: Name -> Exp -> WriterT [Constr] TCM Ty
typeLam var body = do
  argTy <- TVar Nothing Flexible <$> freshName
  resultTy <- local (M.insert var argTy) (typeOf body)
  return (TArr argTy resultTy)

typeOf :: Exp -> WriterT [Constr] TCM Ty
typeOf Num {} = return TNum
typeOf Op{} = return $ TNum `TArr` (TNum `TArr` TNum)
typeOf (Var v) = lookupVar v
typeOf (Lam [] var body) = typeLam var body
typeOf Lam{} = throwError . Impossible $ "Nontrivial closure in typechecking!"
typeOf (App l r) = do
  funTy <- typeOf l
  argTy <- typeOf r
  let tvar = TVar Nothing Flexible -- A new tvar
  [lvar, rvar] <- map tvar <$> sequence [freshName, freshName]
  tell [lvar `TArr` rvar :~: funTy, lvar :~: argTy]
  return rvar

typeGlobal :: M.Map Name Ty -> Def -> CompilerM Def
typeGlobal globals d@Def{..} = flip runReaderT globals $ do
  (ty, constr) <- runWriterT $ typeOf defBody
  sub <- unify constr
  _ <- unify [useSubst ty sub :~: defTy] -- If this succeeds then we're OK
  return d

typeCheck :: [Def] -> CompilerM [Def]
typeCheck defs = let globals = M.fromList $ zipWith annotPair (map defName defs) (map defTy defs)
                 in mapM (typeGlobal globals) defs
  where annotPair n t = (n, annot n t)
