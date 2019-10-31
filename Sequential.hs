module Sequential where

import DataTypes

execute :: Statement -> Int -> SeqExecContext
execute program steps = run (initiate program) steps

run :: SeqExecContext -> Int -> SeqExecContext
run currExecContext steps = if (isTerminated currExecContext || steps == 0) then currExecContext
    else 
    let stack  = currStack currExecContext
        store  = currStore currExecContext
        currStmt = fst (top stack)
        currEnv  = snd (top stack)
    in case currStmt
    of  Nop -> run (SEC (pop stack) store) (steps - 1)
        Var x stmt -> run (newID x stmt currExecContext) (steps - 1)
        VarBind x y -> if (isAbsent x currEnv || isAbsent y currEnv)
                       then error (show x ++ " or " ++ show y ++ " not in scope")
                       else run (SEC (pop stack) (bindIDs x y currEnv store)) (steps - 1)
        ValBind x val -> if (isAbsent x currEnv)
                       then error (show x ++ " not in scope")
                       else run (SEC (pop stack) (bindVarVal var val currEnv store)) (steps - 1)
                            where var = varOfID x currEnv
        Conditional x s1 s2 -> if (isAbsent x currEnv)
                               then error (show x ++ " not in scope") else
                               if (isUnbound (varOfID x currEnv) store)
                               then error (show x ++ " not bound to a value") else
                               if (isBoolean x currEnv store) == False
                               then error (show x ++ " not a boolean") else
                               run (evaluateConditional x s1 s2 stack store) (steps - 1)
        Apply func inputs -> if (rmdups (freeIdentifiers (Apply func inputs) currEnv)) /= []
                             then error "ID not in scope" else
                             if (isUnbound (varOfID func currEnv) store)
                             then error (show func ++ " not bound to a value") else
                             if (isProc func currEnv store) == False
                             then error (show func ++ " not a proc") else
                             run (pushProc func inputs stack store) (steps - 1)
        Statement s -> run (SEC newStack store) (steps - 1)
                       where newStack = (foldr (\p pes -> push (p,currEnv) pes) (pop stack) s)