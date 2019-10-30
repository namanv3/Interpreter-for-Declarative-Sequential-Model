module DataTypes where

import Data.List
import qualified Data.Map as Map

data Statement =  Nop
                | Var Identifier Statement
                | VarBind Identifier Identifier
                | ValBind Identifier Value
                | Conditional Identifier Statement Statement
                | Apply Identifier [Identifier]
                | Statement [Statement]
                deriving(Eq)

data Value =  Nil
            | NumLiteral Int
            | BoolLiteral Bool
            | Proc [Identifier] Statement
            | ProcStore [Identifier] Statement Environment
            deriving (Eq)

data Identifier = Ident String deriving (Eq)

data SAS = SAS { bound   :: [(Variable, Value)]
               , unbound :: [(Variable, EqClass)]
               , unused  :: [Variable]
               }

data Variable = Variable Int deriving (Eq, Ord)

data EqClass = EqClass Int deriving (Eq)

data Environment = Environment [(Identifier,Variable)] deriving(Eq)

data SemanticStack = SemanticStack [(Statement,Environment)]

data MultiStack = MultiStack [SemanticStack]

data SeqExecContext = SEC SemanticStack SAS

data ConExecContext = CEC MultiStack SAS