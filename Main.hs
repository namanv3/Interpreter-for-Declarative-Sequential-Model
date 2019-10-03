import DataTypes
import qualified Data.Map as Map

data ExecutionContext = EC {currStack :: SStack, currSAS :: SAS}

instance Show (ExecutionContext) where
    show (EC stack store) = "STACK:\n" ++ (show stack) ++ "\nSTORE:\n" ++ (show store)


runC :: SStack -> SAS -> Int -> ExecutionContext
runC stack store steps = 
    if (isEmpty stack || (steps == 0)) then EC stack store
    else 
    let stmt = fst (top stack)
        env  = snd (top stack)
    in case stmt 
    of (Nop)         -> runC (pop stack) store (steps - 1)
       (Var id prog) -> runC newStack newSAS (steps - 1)
                        where newEnv   = fst (adjoin env store id)
                              newSAS   = snd (adjoin env store id)
                              newStack = push (prog,newEnv) (pop stack)
       (VarBind i j) -> if (idIsAbsent env i) || (idIsAbsent env i) 
                        then error "ID not in scope"
                        else runC (pop stack) (bindVarVar store v1 v2) (steps - 1)
                             where v1 = varInEnv env i
                                   v2 = varInEnv env j
       (ValBind i v) -> if (idIsAbsent env i) || (checkValue v == False)
                        then error "ID not in scope"
                        else case v
                             of (Literal n)   -> runC (pop stack) (bindVarVal store var v) (steps - 1)
                                                 where var = varInEnv env i
                                (Record n ls) -> runC (pop stack) (bindVarVal store var val) (steps - 1)
                                                 where var = varInEnv env i
                                                       val = convertToStore (Record n ls) env
                                (Proc prms s) -> runC (pop stack) (bindVarVal store var val) (steps - 1)
                                                 where var = varInEnv env i
                                                       val = convertToStore (Proc prms s) env
       (Conditional i s1 s2) -> if (idIsAbsent env i)
                                then error "ID not in scope" else
                                if (Map.member (varInEnv env i) (unbound store)) then error "ID not bound to a value" else
                                if isLiteral (valueOf i env store) == False then error "ID refers to a non literal value" else
                                case (valueOf i env store) of (Literal 0) -> runC newStack store (steps - 1)
                                                                             where newStack = push (s2,env) (pop stack)
                                                              (Literal _) -> runC newStack store (steps - 1)
                                                                             where newStack = push (s1,env) (pop stack)
       (Apply name inputs) -> if (freeIdentifiers (Apply name inputs) env) /= []
                              then error "ID not in scope" else
                              if (Map.member (varInEnv env name) (unbound store)) then error "ID not bound to a value" else
                              if isProcedure (valueOf name env store) == False then error "ID refers to a non procedure value" else
                              if length inputs /= numArgs (valueOf name env store) then error "number of args dont match" else
                              runC newStack store (steps - 1)
                              where proc = valueOf name env store
                                    newEnv = foldl (\e p -> insertID e (fst p,varInEnv env (snd p))) (contextualEnv proc) (zip (allArgs proc) inputs)
                                    newStack = push (stmtOfProc (valueOf name env store),newEnv) (pop stack)
       (Match i pattern s1 s2) -> if (freeIdentifiers (Match i pattern s1 s2) env) /= []
                                  then error "ID not in scope" else
                                  if isRecord (valueOf i env store) == False  then error "ID refers to a non record value" else
                                  if recordsMatch (valueOf i env store) pattern then
                                  let newIDs = map snd (recFeatures pattern)
                                      newVars = map snd (recStoreFeatures (valueOf i env store))
                                      newEnv = foldl (\e p -> insertID e p) env (zip newIDs newVars)
                                      newStack = push (s1,newEnv) (pop stack)
                                  in runC newStack store (steps - 1)
                                  else runC (push (s2,env) (pop stack)) store (steps - 1)
       (Statement s) -> runC newStack store (steps - 1)
                        where newStack = (foldr (\p pes -> push (p,env) pes) (pop stack) s)

x = Ident "x"
y = Ident "y"
z = Ident "z"
a = Ident "a"
b = Ident "b"
c = Ident "c"
p = Ident "p"
q = Ident "q"
r = Ident "r"
assign = Ident "assign"

l0 = Literal 0
l1 = Literal 1
l2 = Literal 2

l12 = Literal 12
l13 = Literal 13
l14 = Literal 14

r0 = Record l12 [(l13,a), (l14,b)]
r1 = Record l12 [(l13,z), (l14,c)]
r2 = Record l12 [(l14,c)]
r3 = Record l13 [(l14,c)]
r4 = Record l13 [(l14,x)]

proc0 = Proc [a, b, c] (ValBind a l12)
proc1 = Proc [a, b, c] (VarBind a x)
proc2 = Proc [a] (ValBind a l12)
proc3 = Proc [a] (VarBind a x)
proc4 = Proc [a,b] (VarBind a b)

p0  = Nop
p1  = nopList 8
p2  = Var a Nop
p3  = Var x (VarBind x y)        --- should result in an error
p4  = Var x (Var y (VarBind x y))
p5  = Var x (Var y (Statement [VarBind x y,ValBind x l12]))
p6  = Var x (Var y (Statement [VarBind x y,ValBind y l12]))
p7  = Var x (Statement [ValBind x l12, ValBind x l13])       --- should result in an error
p8  = Var x (Var y (ValBind x r0))
p9  = Var x (Var y (Statement [ValBind x r0,ValBind y l12]))
p10 = Var x (Var y (Var z (Var a (Var b (Var c (Statement [ValBind x r0, ValBind y r1, ValBind a l0, ValBind b l1, VarBind x y]))))))
p11 = Var x (Var y (Var z (Var a (Var b (Var c (
        Statement [ValBind x r0, ValBind y r1, ValBind a l0, ValBind z l1, VarBind x y])))))) --- should result in an error
p12 = Var x (Statement [ValBind x l1, Var x (ValBind x l2)])
p13 = Var x (Statement [ValBind x l1, Var x (ValBind x l2), Var y (Statement [Nop, VarBind x y])])
p14 = Var x (Var y (Var a (Var b (Var c (
        Statement [ValBind x r0, ValBind y r1, VarBind x y]))))) --- should result in an error
p15 = Var x (Var y (Var a (Var b (Var c (
        Statement [ValBind x r0, ValBind y r2, VarBind x y]))))) --- should result in an error
p16 = Var x (Var y (Var a (Var b (Var c (
        Statement [ValBind x r3, ValBind y r2, VarBind x y]))))) --- should result in an error
p17 = Var x (ValBind x proc0)
p18 = Var x (Var y (ValBind y proc1))
p19 = Var x (Statement [ValBind x r4, Nop])
p20 = Var a (Var y (ValBind y proc0))
p21 = Var y (
            Var x (
                Statement [
                    ValBind x l12,
                    Conditional x (ValBind y l12) Nop 
                ]
            )
    )
p22 = Var y (
            Statement [
                ValBind y proc0,
                Var x (
                    Apply y [x]
                )
            ]
    )
p23 = Var y (
            Statement [
                ValBind y proc2,
                Var x (
                    Var z (
                        Statement [
                            Apply y [x],
                            VarBind z x,
                            Nop
                        ]

                    )
                )
            ]
    )
p24 = Var y (
        Statement [
            ValBind y proc3,
            Var x (
                Apply y [x]
            )
        ]
    )
p25 = Var assign (
        Var x (
            Var y (
                Statement [
                    ValBind assign proc4,
                    Apply assign [x,y],
                    Nop
                ]
            )
        )
    )
p26 = Var assign (
        Var x (
            Var y (
                Statement [
                    ValBind assign proc4,
                    ValBind x l12,
                    ValBind y l13,
                    Apply assign [x,y],
                    Nop
                ]
            )
        )
    )
p27 = Var p (
        Var q (
            Var a (
                Var b (
                    Statement [
                        ValBind q r0,
                        Match q r1 (ValBind p l1) (ValBind p l2),
                        Match q r1 (ValBind z l1) (ValBind c l2),
                        -- Match q r3 (ValBind p l1) (ValBind c l2),   --- this will give an error
                        Match q r3 (ValBind p l1) (ValBind b l2),
                        Nop
                    ]
                )
            )
        )
    )