{-# LANGUAGE OverloadedStrings #-}
module Language.Hasquito.AbsMachine where
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map as M
import           Language.Hasquito.STG
import           Language.Hasquito.Syntax
import           Language.Hasquito.Util

type Address = Int

data Reg = SpA -- ^ stack for ints
         | SpB -- ^ stack for closures addresses
         | SpC -- ^ stack for continuations
         | SpF -- ^ stack for update frames
         | BpA -- ^ saving stack value of SpA
         | BpB -- ^ saving stack value of SpB
         | Node -- ^ Current closure
         | Ret -- ^ Return register for integers
         | User Int -- ^ A user defined register

data Stmt = WriteStack Reg Int Reg -- ^ Push an prim/address onto a stack
          | ReadStack Name Reg Reg -- ^ Assign the Int th argument to Name
          | AdjustSP Reg Int       -- ^ Increment or decrement a SP
          | Enter Address          -- ^ Enter a closure at a particular addr
          | AllocC Reg  Closure    -- ^ Allocate a closure, store result
          | AllocF Reg  Frame      -- ^ Allocate a frame, store the result
          | PrimOp Op Reg Reg Reg  -- ^ Do a primitive operation

type Program = [Stmt]

data Closure = Closure { entry  :: Address -- ^ Address of entry code
                       , closed :: Int   }
data Frame = Frame {closurePtr :: Int}

data HeapEntry = HeapFrame Frame
               | HeapClosure Closure
               | HeapProgram Program

type HeapEntries  = M.Map Name Address
type HeapContents = M.Map Int HeapEntry
type StackEntries = M.Map Name Int
data HeapState = HeapState { entries  :: HeapEntries
                           , contents :: HeapContents
                           , stackEnt :: StackEntries
                           , currAddr :: Address}
type HeapM = StateT HeapState CompilerM

alloc :: Int -> HeapM Address
alloc i = gets currAddr <* modify bump
  where bump h = h{currAddr = currAddr h + i}

place :: Maybe Name -> HeapEntry -> Address -> HeapM ()
place mayName entry address = do
  modify $ \h -> h{contents = M.insert address entry (contents h)}
  case mayName of
    Just name -> modify $ \h -> h{entries = M.insert name address (entries h)}
    Nothing   -> return ()

allocProg :: (Address -> HeapM Program) -> HeapM Address
allocProg mkProg = do
  heapState <- get -- Get the world before mkProg
  size <- length <$> mkProg (error "Cannot depend on program address for size")
  put heapState   -- No side effects from running mkProg
  address <- alloc size
  program <- mkProg address
  place Nothing (HeapProgram program) address
  return address

getName :: Name -> HeapM Address
getName n = do
  entry <- M.lookup n <$> gets entries
  case entry of
    Nothing -> lift . throwError . Impossible $ "Unbound name in STG compilation!"
    Just a  -> return a

putName :: Name -> Address -> HeapM ()
putName name address = modify $ \h -> h{entries = M.insert name address (entries h)}

compileSExp :: SExp -> Address -> HeapM Program
compileSExp = undefined

compileSTG :: TopLevel -> HeapM ()
compileSTG (Thunk name sexp) = do
  entryCode <- allocProg $ compileSExp sexp
  address <- alloc 1
  place (Just name) (HeapClosure $ Closure entryCode 0) address
compileSTG (Fun name closed vars body) = do
  address <- alloc (1 + length closed)
  -- Save the stuff we're about to clobber
  overwritten <- filterM (\n -> M.member n <$> gets entries) closed
  prevEntries <- mapM getName overwritten
  prevStack <- gets stackEnt
  -- Clobber
  forM_ (zip closed [1..]) $ \(name, offset) ->
    putName name $ address + offset
  modify $ \h -> h{stackEnt = M.fromList $ zip vars [0..]}
  -- Actual code gen
  entryCode <- allocProg $ compileSExp body
  place (Just name) (HeapClosure $ Closure entryCode (length closed)) address
  -- Restore previous state
  modify $ \h -> h{stackEnt = prevStack}
  forM_ (zip overwritten prevEntries) (uncurry putName)
