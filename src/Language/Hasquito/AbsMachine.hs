module Language.Hasquito.AbsMachine where
import Language.Hasquito.Syntax

data Reg = SpA -- ^ stack for ints
         | SpB -- ^ stack for closures addresses
         | SpC -- ^ stack for continuations
         | SpF -- ^ stack for update frames
         | BpA -- ^ saving stack value of SpA
         | BpB -- ^ saving stack value of SpB
         | Node -- ^ Current closure
         | Ret -- ^ Return register for integers
         | User Int -- ^ A user defined register

data Stmt = WriteStack Reg Int Int -- ^ Push an prim/address onto a stack
          | ReadStack Name Reg Int -- ^ Assign the Int th argument to Name
          | AdjustSP Reg Int       -- ^ Increment or decrement a SP
          | Enter Int              -- ^ Enter a closure at a particular addr
          | AllocC Name Closure    -- ^ Allocate a closure, store result
          | AllocF Name Frame      -- ^ Allocate a frame, store the result
          | PrimOp Op Reg Reg Reg  -- ^ Do a primitive operation

type Program = [Stmt]

data Closure = Closure { entry  :: Program
                       , closed :: [Int]   }
data Frame = Frame {closurePtr :: Int}
