module DataTypes where

import Data.List
import qualified Data.Map as Map

data Identifier = Ident String deriving (Eq,Ord)

instance Show Identifier where
    show (Ident x) = "Ident(" ++ x ++ ")"

data Value =  Nil 
            | Literal Int
            | Record Value [(Value, Identifier)] -- but here, the values need to be Literals
            | RecordStore Value [(Value, Variable)] -- but here, the values need to be Literals
            | Proc [Identifier] Statement
            | ProcStore [Identifier] Statement Environment 
            deriving (Eq,Show)

isSameValType :: Value -> Value -> Bool
isSameValType Nil Nil = True
isSameValType (Literal _) (Literal _) = True
isSameValType (Record _ _) (Record _ _) = True
isSameValType (RecordStore _ _) (RecordStore _ _) = True
isSameValType (Proc _ _) (Proc _ _) = True
isSameValType (ProcStore _ _ _) (ProcStore _ _ _) = True
isSameValType _ _ = False


data Variable = NoVariable
                | Variable Int deriving (Eq, Ord, Show)

data EqClass = EqClass Int deriving (Eq, Show)

newEqClass :: Variable -> EqClass
newEqClass (Variable n) = (EqClass n)

data Statement =  Nop
                | Var Identifier Statement
                | VarBind Identifier Identifier
                | ValBind Identifier Value 
                | Statement [Statement]
                deriving(Eq,Show)

isLiteral :: Value -> Bool
isLiteral (Literal _) = True
isLiteral _ = False

checkValue :: Value -> Bool
checkValue (Record (Literal _) vis) = (length (filter (\(v,i) -> isLiteral v) vis)) == (length vis)
checkValue (RecordStore (Literal _) vis) = (length (filter (\(v,i) -> isLiteral v) vis)) == (length vis)
checkValue Nil = True
checkValue (Literal _) = True
checkValue (Proc _ _) = True
checkValue (ProcStore _ _ _) = True
checkValue _ = False

nopList :: Int -> Statement
nopList a = Statement (take a (cycle [Nop]))

n :: Int
n = 10 -- max size of SAS

entryLabels :: [(Value,a)] -> [Value]                       -- works for both records and recordstores2
entryLabels entries = foldr (\p ls-> (fst p):ls) [] entries

entryVariables :: [(Value,Variable)] -> [Variable]
entryVariables entries = foldr (\p ls-> (snd p):ls) [] entries

-------------------------------------------------------------------------------------------

data SAS = SAS { bound   :: Map.Map Variable Value
               , unbound :: Map.Map Variable EqClass
               , unused  :: [Variable]
               }

instance Show SAS where
    show sas = "bound: " ++ (show (Map.toList (bound sas))) ++ "\nunbound: " ++ (show (Map.toList (unbound sas)))

bindEqVal :: SAS -> EqClass -> Value -> SAS
bindEqVal sas eq val = 
    if checkValue val == False
    then error "invalid Record value"
    else let newBoundVars = Map.keys (Map.filter (== eq) (unbound sas))
             newBoundMap  = foldl (\m var -> Map.insert var val m) (bound sas) newBoundVars
         in (SAS newBoundMap (Map.filter (/= eq) (unbound sas)) (unused sas))

bindEqEq :: SAS -> EqClass -> EqClass -> SAS
bindEqEq sas e1 e2 = if e1 == e2 then sas else
    let newE1Vars = Map.keys (Map.filter (== e2) (unbound sas))
        restOfMap = Map.filter (/= e2) (unbound sas)
        newUnbound = foldl (\m var -> Map.insert var e1 m) restOfMap newE1Vars
    in (SAS (bound sas) newUnbound (unused sas))

isUnbound :: SAS -> Variable -> Bool
isUnbound sas var = Map.lookup var (bound sas) == Nothing

isbound :: SAS -> Variable -> Bool
isbound sas var = Map.lookup var (unbound sas) == Nothing

findEqClass :: SAS -> Variable -> EqClass
findEqClass sas var = case (Map.lookup var (unbound sas)) of (Just e) -> e

findValue :: SAS -> Variable -> Value
findValue sas var = case (Map.lookup var (bound sas)) of (Just v) -> v

unify :: SAS -> Variable -> Variable -> SAS
unify sas v1 v2
    | isUnbound sas v1 && isUnbound sas v2 = bindEqEq sas (findEqClass sas v1) (findEqClass sas v2)
    | isUnbound sas v1 = bindEqVal sas (findEqClass sas v1) (findValue sas v2)
    | isUnbound sas v2 = bindEqVal sas (findEqClass sas v2) (findValue sas v1)

unify sas v1 v2 =
    let val1 = findValue sas v1
        val2 = findValue sas v2
    in if (isSameValType val1 val2)
       then case (val1,val2)
            of (RecordStore name1 entries1, RecordStore name2 entries2) -> (
                    let labels1 = entryLabels entries1
                        labels2 = entryLabels entries2
                        tobeUnified = zip (entryVariables entries1) (entryVariables entries2)
                    in if name1 /= name2 then error "Record entries not compatible"
                       else if labels1 /= labels2 then error "Record entries not compatible"
                       else foldl (\store p -> unify store (fst p) (snd p)) sas tobeUnified
                )
               (Literal a, Literal b) -> if a == b then sas else error "Both variables are already assigned different values"
       else error "Different data types being matched"

findNewVar :: SAS -> (Variable,SAS)
findNewVar (SAS b ub un) = if length un == 0 then error "Max storage of SAS reached"
                           else let newvar = head un
                                    newlist = tail un
                                    newubmap = Map.insert newvar (newEqClass newvar) ub
                                in (newvar, (SAS b newubmap newlist))

bindVarVar :: SAS -> Variable -> Variable -> SAS
bindVarVar sas v1 v2 = unify sas v1 v2

bindVarVal :: SAS -> Variable -> Value -> SAS
bindVarVal sas var val
    | isbound sas var = error ("variable " ++ (show var) ++ " already assigned a value")
    | otherwise = let eq = case (Map.lookup var (unbound sas)) of (Just e) -> e
                  in bindEqVal sas eq val

emptySAS = (SAS Map.empty Map.empty (map Variable [1..n]))

-------------------------------------------------------------------------------------------------------------

data Environment = Environment {mapIDVar :: Map.Map Identifier Variable} deriving(Eq)

instance Show Environment where
    show = show . (Map.toList) . mapIDVar

idIsAbsent :: Environment -> Identifier -> Bool
idIsAbsent env i = Map.lookup i (mapIDVar env) == Nothing

varInEnv :: Environment -> Identifier -> Variable
varInEnv env i = case (Map.lookup i (mapIDVar env)) of (Just v) -> v
                                                       Nothing  -> error "Identifier out of scope"

adjoin :: Environment -> SAS -> Identifier -> (Environment,SAS)
adjoin env sas i =
    let vsPair = findNewVar sas
        var = fst vsPair
        newEnv = Map.insert i var (mapIDVar env)
    in (Environment newEnv,snd vsPair)

restrictEnv :: Environment -> [Identifier] -> Environment
restrictEnv env idList = 
    let listEnv = Map.toList (mapIDVar env)
        selected = filter (\pair -> fst pair `elem` idList) listEnv
    in Environment $ Map.fromList selected

freeIdentifiers :: Statement -> Environment -> [Identifier]
freeIdentifiers Nop env = []
freeIdentifiers (Var id stmt) env = filter (/= id) (freeIdentifiers stmt env)
freeIdentifiers (VarBind i j) env = 
    let listI = if idIsAbsent env i then [i] else []
        listJ = if idIsAbsent env j then [j] else []
    in listI ++ listJ
freeIdentifiers (ValBind i _) env = if idIsAbsent env i then [i] else []
freeIdentifiers (Statement s) env = foldl (\all s -> freeIdentifiers s env ++ all) [] s


convertToStore :: Value -> Environment -> Value
convertToStore (Record name list) env = 
    let absentIDs = filter (\pair -> idIsAbsent env (snd pair)) list
        isNoError = absentIDs == []
        featVarPairs = map (\pair -> (fst pair, varInEnv env (snd pair))) list
    in if isNoError then (RecordStore name featVarPairs) else error "Undeclared Identifiers in Record"
convertToStore (Proc parameters stmt) env =
    let allIds = freeIdentifiers stmt emptyEnv
        externalIds = allIds \\ parameters
    in (ProcStore parameters stmt (restrictEnv env externalIds))


emptyEnv = Environment Map.empty

-------------------------------------------------------------------------------------------------------------

data SStack = SStack {execStack :: [(Statement,Environment)]}

instance Show SStack where
    show (SStack ses) = 
        let endWithNL = map ((++ "\n") . show) ses
        in foldl (++) "" endWithNL 

pop :: SStack -> SStack
pop (SStack s) =  SStack (tail s)

top :: SStack -> (Statement,Environment)
top (SStack s) = head s

push :: (Statement,Environment) -> SStack -> SStack
push p (SStack s) = SStack (p:s)

currStmt :: SStack -> Statement
currStmt s = fst (top s)

startStack :: Statement -> SStack
startStack program = SStack [(program,emptyEnv)]

isEmpty :: SStack -> Bool
isEmpty (SStack s) = (length s) == 0