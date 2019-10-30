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
                       then error "Identifier not in scope"
                       else run (SEC (pop stack) (bindIDs x y currEnv store)) (steps - 1)
        ValBind x val -> if (isAbsent x currEnv)
                       then error "Identifier not in scope"
                       else run (SEC (pop stack) (bindVarVal var val store)) (steps - 1)
                            where var = varOfID x currEnv
        Conditional x s1 s2 -> if (isAbsent x currEnv)
                               then error "Identifier not in scope" else
                               if (isUnbound (varOfID x currEnv) store)
                               then error "Identifier not bound to a value" else
                               if (isBoolean x currEnv store) == False
                               then error "Identifier not a boolean" else
                               run (evaluateConditional x s1 s2 stack store) (steps - 1)
        Statement s -> run (SEC newStack store) (steps - 1)
                       where newStack = (foldr (\p pes -> push (p,currEnv) pes) (pop stack) s)