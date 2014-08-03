module Language.Hasquito.AbsMachine where
import           Control.Applicative
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
                       , closed :: [Int]   }
data Frame = Frame {closurePtr :: Int}

data HeapEntry = HeapFrame Frame
               | HeapClosure Closure
               | HeapProgram Program

type HeapEntries  = M.Map Name Int
type HeapContents = M.Map Int HeapEntry

data HeapState = HeapState { entries  :: HeapEntries
                           , contents :: HeapContents
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

allocProg :: (Address -> Program) -> HeapM ()
allocProg prog = do
  address <- alloc size
  place Nothing (HeapProgram $ prog address) address
  where size = length . prog $ error "Program size can't depend on address"
