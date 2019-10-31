module Concurrent where

import DataTypes
import Sequential

conExecute :: Statement -> Int -> ConExecContext
conExecute program steps = runC (conInitiate program) steps

runC :: ConExecContext -> Int -> ConExecContext
runC context steps = if (noThreadsLeft context || steps == 0) then context
    else 
    let stack = chooseThread context
        store = getStore context
        currStmt = fst (top stack)
        currEnv  = snd (top stack)
    in case currStmt
    of  Nop -> runC (updateMStack (pop stack) context) (steps - 1)

        Var x stmt -> runC (addID x context) (steps - 1)

        VarBind x y ->  if (isAbsent x currEnv || isAbsent y currEnv)
                        then error (show x ++ " or " ++ show y ++ " not in scope")
                        else runC (updateMStack (pop stack) (updateStore (bindIDs x y currEnv store) context)) (steps - 1)

        ValBind x val ->if (isAbsent x currEnv)
                        then error (show x ++ " not in scope")
                        else runC (updateMStack (pop stack) (updateStore (bindVarVal var val currEnv store) context)) (steps - 1)
                        where var = varOfID x currEnv

        Conditional x s1 s2 ->  if (isAbsent x currEnv)
                                then error (show x ++ " not in scope") else
                                if (isUnbound (varOfID x currEnv) store)
                                then runC (switchThread context) (steps - 1) else
                                if (isBoolean x currEnv store) == False
                                then error (show x ++ " not a boolean") else
                                runC (evaluateConditionalC x s1 s2 context) (steps - 1)

        Apply func inputs ->if (rmdups (freeIdentifiers (Apply func inputs) currEnv)) /= []
                            then error "ID not in scope" else
                            if (isUnbound (varOfID func currEnv) store)
                            then runC (switchThread context) (steps - 1) else
                            if (isProc func currEnv store) == False
                            then error (show func ++ " not a proc") else
                            runC (applyProc func inputs context) (steps - 1)

        Match x pattern s1 s2 ->if isAbsent x currEnv
                                then error (show x ++ " not in scope") else
                                if (isUnbound (varOfID x currEnv) store)
                                then runC (switchThread context) (steps - 1) else
                                if (isRecordValue (valueOfID x currEnv store)) == False
                                then error (show x ++ " not a record") else
                                runC (patternMatchC x pattern s1 s2 context) (steps - 1)

        Sum x y z ->    if (isAbsent x currEnv) || (isAbsent y currEnv) || (isAbsent z currEnv)
                        then error ("One out of" ++ show [x,y,z] ++ "not in scope") else
                        if isUnbound (varOfID x currEnv) store || isUnbound (varOfID x currEnv) store
                        then runC (switchThread context) (steps - 1) else
                        runC (updateMStack (pop stack) (updateStore (bindVarVal (varOfID z currEnv) (add x y store currEnv) currEnv store) context)) (steps - 1)

        Product x y z ->if (isAbsent x currEnv) || (isAbsent y currEnv) || (isAbsent z currEnv)
                        then error ("One out of" ++ show [x,y,z] ++ "not in scope") else
                        if isUnbound (varOfID x currEnv) store || isUnbound (varOfID x currEnv) store
                        then runC (switchThread context) (steps - 1) else
                        runC (updateMStack (pop stack) (updateStore (bindVarVal (varOfID z currEnv) (multiply x y store currEnv) currEnv store) context)) (steps - 1)

        Thread s -> runC (addThread (s,currEnv) (updateMStack (pop stack) context)) (steps - 1)

        Statement s ->  runC (updateMStack newStack context) (steps - 1)
                        where newStack = (foldr (\p pes -> push (p,currEnv) pes) (pop stack) s)












