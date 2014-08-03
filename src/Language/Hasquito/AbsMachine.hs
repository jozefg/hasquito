module Language.Hasquito.AbsMachine where
import           Control.Monad.State
import qualified Data.Map as M
import           Language.Hasquito.STG
import           Language.Hasquito.Syntax
import           Language.Hasquito.Util

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
          | Enter Int              -- ^ Enter a closure at a particular addr
          | AllocC Reg  Closure    -- ^ Allocate a closure, store result
          | AllocF Reg  Frame      -- ^ Allocate a frame, store the result
          | PrimOp Op Reg Reg Reg  -- ^ Do a primitive operation
          | CurAddress Reg         -- ^ 

type Program = [Stmt]

data Closure = Closure { entry  :: Program
                       , closed :: [Int]   }
data Frame = Frame {closurePtr :: Int}

type Heap  = M.Map Name Int
type HeapM = StateT Heap CompilerM
