module Language.Hasquito.AbsMachine where
import Language.Hasquito.Syntax

data Reg = SpA | SpB | SpC | SpF | Node | User Int

data Stmt = WriteStack Reg Int Int -- ^ Push an prim/address onto a stack
          | ReadStack Name Reg Int -- ^ Assign the Int th argument to Name
          | AdjustSP Reg Int       -- ^ Increment or decrement a SP
          | Enter Int              -- ^ Enter a closure at a particular addr

type Program = [Stmt]


data InfoTable = Info { entryCode :: Program }
data Closure = Closure { info   :: InfoTable
                       , closed :: [Int] }
