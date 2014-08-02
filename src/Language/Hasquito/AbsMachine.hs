module Language.Hasquito.AbsMachine where
import Language.Hasquito.Syntax

data Reg = SpA | SpB | SpC | SpF | BpA | BpB | Node | User Int

data Stmt = WriteStack Reg Int Int -- ^ Push an prim/address onto a stack
          | ReadStack Name Reg Int -- ^ Assign the Int th argument to Name
          | AdjustSP Reg Int       -- ^ Increment or decrement a SP
          | Enter Int              -- ^ Enter a closure at a particular addr
          | AllocC Name Closure    -- ^ Allocate a closure, store result
          | AllocF Name Frame      -- ^ Allocate a frame, store the result

type Program = [Stmt]


data InfoTable = Info { entryCode :: Program }
data Closure = Closure { info   :: InfoTable
                       , closed :: [Int] }
data Frame = Frame {closurePtr :: Int}
