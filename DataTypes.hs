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
                deriving(Eq,Show)

data Value =  Nil
            | NumLiteral Int
            | BoolLiteral Bool
            | Proc [Identifier] Statement
            | ProcStore [Identifier] Statement Environment
            deriving (Eq)

instance Show Value where
    show Nil = "Nil"
    show (NumLiteral i)  = "Num("  ++ show i ++ ")"
    show (BoolLiteral i) = "Bool(" ++ show i ++ ")"
    show _ = ""

data Identifier = Ident String deriving (Eq)

instance Show Identifier where
    show (Ident x) = "Ident(" ++ x ++ ")"

data SAS = SAS { bound   :: [(Variable, Value)]
               , unbound :: [(Variable, EqClass)]
               , unused  :: [Variable]
               }

instance Show SAS where
    show sas = "bound:\n" ++ b ++ "\nunbound:\n" ++ ub
               where b  = foldl (\str pair-> str ++ show (fst pair) ++ " = " ++ show (snd pair) ++ "\n") "" (bound sas)
                     ub = foldl (\str pair-> str ++ show (fst pair) ++ " : " ++ show (snd pair) ++ "\n") "" (unbound sas)

data Variable = Variable Int deriving (Eq, Ord)

instance Show Variable where
    show (Variable x) = "Var(" ++ show x ++ ")"

data EqClass = EqClass Int deriving (Eq)

instance Show EqClass where
    show (EqClass x) = "Eq(" ++ show  x ++ ")"

data Environment = Environment [(Identifier,Variable)] deriving(Eq)

instance Show Environment where
    show (Environment e) = foldl (\str pair-> str ++ show (fst pair) ++ " : " ++ show (snd pair) ++ "\n") "" e

data SemanticStack = SemanticStack [(Statement,Environment)]

instance Show SemanticStack where
    show (SemanticStack s) = foldl (\str p-> str ++ "(\n" ++ show (fst p) ++ "\n" ++ show (snd p) ++ ")\n") "" s

data MultiStack = MultiStack [SemanticStack]

data SeqExecContext = SEC {currStack::SemanticStack, currStore::SAS}

instance Show SeqExecContext where
    show (SEC stack store) = (show stack) ++ "\n\n" ++ (show store)

data ConExecContext = CEC MultiStack SAS

---------------------------------------------------------------------------------------------------
-- Statement functions

---------------------------------------------------------------------------------------------------
-- Value functions

isBooleanValue :: Value -> Bool
isBooleanValue (BoolLiteral _) = True
isBooleanValue v = False

---------------------------------------------------------------------------------------------------
-- ID functions

---------------------------------------------------------------------------------------------------
-- SAS functions

n = 100 -- max number of variables

emptyStore = SAS [] [] (map Variable [1..n])

newEqClass :: Variable -> EqClass
newEqClass (Variable i) = EqClass i

findEqClass :: Variable -> SAS -> EqClass
findEqClass v store = snd (head [pair | pair <- (unbound store), (fst pair) == v])

isUnbound :: Variable -> SAS -> Bool
isUnbound v store = (filter (\pair -> (fst pair) == v) (unbound store)) /= []

valueOf :: Variable -> SAS -> Value
valueOf v store = snd (head [pair | pair <- (bound store), (fst pair) == v])

createNewVar :: SAS -> (Variable,SAS)
createNewVar (SAS b ub uu) = (head uu,SAS b ((head uu,newEqClass (head uu)):ub) (tail uu))

bindIDs :: Identifier -> Identifier -> Environment -> SAS -> SAS
bindIDs x y env store = bindVarVar (varOfID x env) (varOfID y env) store

bindVarVar :: Variable -> Variable -> SAS -> SAS
bindVarVar v1 v2 store = unify v1 v2 store

bindVarVal :: Variable -> Value -> SAS -> SAS
bindVarVal var val store = if isUnbound var store then bindEqVal (findEqClass var store) val store else 
                           if (valueOf var store) == val then store
                           else error (show var ++ " bound to a different value")

bindEqVal :: EqClass -> Value -> SAS -> SAS
bindEqVal eq val store = 
    let newBoundVars = map fst [pair | pair <- (unbound store), (snd pair) == eq]
        newBound = foldl (\b var -> (var,val):b) (bound store) newBoundVars
        newUnbound = filter (\pair -> (snd pair) /= eq) (unbound store)
    in SAS newBound newUnbound (unused store)

bindEqEq :: EqClass -> EqClass -> SAS -> SAS
bindEqEq e1 e2 store = if e1 == e2 then store else
    let e2Vars = map fst [pair | pair <- (unbound store), (snd pair) == e2]
        rest = [pair | pair <- (unbound store), (snd pair) /= e2]
        newUnbound = foldl (\ub var -> (var,e1):ub) rest e2Vars
    in SAS (bound store) newUnbound (unused store)

unify :: Variable -> Variable -> SAS -> SAS
unify v1 v2 store
    | isUnbound v1 store && isUnbound v2 store = bindEqEq (findEqClass v1 store) (findEqClass v2 store) store
    | isUnbound v1 store = bindEqVal (findEqClass v1 store) (valueOf v2 store) store
    | isUnbound v2 store = bindEqVal (findEqClass v2 store) (valueOf v1 store) store

unify v1 v2 store = 
    let val1 = valueOf v1 store
        val2 = valueOf v2 store
    in if val1 == val2 then store else error "Different values being equated"

isBoolean :: Identifier -> Environment -> SAS -> Bool
isBoolean x env store = isBooleanValue (valueOfID x env store)

---------------------------------------------------------------------------------------------------
-- Environment functions

emptyEnv = Environment []

addMapping :: Identifier -> Variable -> Environment -> Environment
addMapping x var (Environment e) = Environment ((x,var):(filter (\p -> (fst p) /= x) e))

varOfID :: Identifier -> Environment -> Variable
varOfID x (Environment e) = snd (head (filter (\pair -> (fst pair) == x) e))

valueOfID :: Identifier -> Environment -> SAS -> Value
valueOfID x env store = valueOf (varOfID x env) store

adjoin :: Identifier -> Environment -> SAS -> (Environment,SAS)
adjoin x env store = 
    let varStorePair = createNewVar store
        newVar = fst varStorePair
        newStore = snd varStorePair
        newEnv = addMapping x newVar env
    in (newEnv,newStore)

isAbsent :: Identifier -> Environment -> Bool
isAbsent x (Environment e) = (filter (\pair -> (fst pair) == x) e) == []

---------------------------------------------------------------------------------------------------
-- SemanticStack functions

newStack :: Statement -> SemanticStack
newStack program = SemanticStack [(program,emptyEnv)]

isEmpty :: SemanticStack -> Bool
isEmpty (SemanticStack s) = (length s) == 0

pop :: SemanticStack -> SemanticStack
pop (SemanticStack s) =  SemanticStack (tail s)

top :: SemanticStack -> (Statement,Environment)
top (SemanticStack s) = head s

push :: (Statement,Environment) -> SemanticStack -> SemanticStack
push p (SemanticStack s) = SemanticStack (p:s)

currStmt :: SemanticStack -> Statement
currStmt s = fst (top s)

currEnv :: SemanticStack -> Environment
currEnv s = snd (top s)

---------------------------------------------------------------------------------------------------
-- SeqExecContext functions

initiate :: Statement -> SeqExecContext
initiate program = SEC (newStack program) emptyStore

isTerminated :: SeqExecContext -> Bool
isTerminated (SEC stack _) = isEmpty stack

newID :: Identifier -> Statement -> SeqExecContext -> SeqExecContext
newID x stmt sec = SEC (push stmtEnv (pop (currStack sec))) newStore
                   where res = adjoin x (currEnv (currStack sec)) (currStore sec)
                         stmtEnv = (stmt,fst res)
                         newStore = snd res

evaluateConditional :: Identifier -> Statement -> Statement -> SemanticStack -> SAS -> SeqExecContext
evaluateConditional x s1 s2 stack store = 
    if (valueOfID x (currEnv stack) store) == (BoolLiteral True) 
    then SEC (push (s1,currEnv stack) (pop stack)) store
    else SEC (push (s2,currEnv stack) (pop stack)) store














