import DataTypes
import Sequential

w = Ident "w"
x = Ident "x"
y = Ident "y"
z = Ident "z"

a = Ident "a"
b = Ident "b"
c = Ident "c"
d = Ident "d"

p = Ident "p"
q = Ident "q"
r = Ident "r"
s = Ident "s"

l0 = NumLiteral 0
l1 = NumLiteral 1
l2 = NumLiteral 2
l3 = NumLiteral 3
l4 = NumLiteral 4

l10  = NumLiteral 10
l20  = NumLiteral 20
l100 = NumLiteral 100
l200 = NumLiteral 200

true  = BoolLiteral True
false = BoolLiteral False

prog0 = 
    Var x (
        Var y (
            Statement[
                Var x (
                    Statement [
                        VarBind x y,
                        VarBind y x,
                        ValBind x l10
                    ]
                ),
                ValBind y l10,
                VarBind x y
            ]
        )
    )

prog1 = 
    Var x (
        Var y (
            Statement[
                Var x (
                    Statement [
                        VarBind x y,
                        ValBind x l10
                    ]
                ),
                VarBind x y,
                ValBind x l10
            ]
        )
    )

prog2 = 
    Var x (
        Var y (
            Statement[
                Var x (
                    Statement [
                        ValBind x true,
                        Conditional x (ValBind y l10) (ValBind y l20)
                    ]
                ),
                VarBind x y,
                ValBind x l10
            ]
        )
    )

prog3 = 
    Var x (
        Var y (
            Statement[
                Var x (
                    Statement [
                        ValBind x l1,
                        Conditional x (ValBind y l10) (ValBind y l20)
                    ]
                ),
                VarBind x y,
                ValBind x l10
            ]
        )
    )

prog4 = 
    Var x (
        Var y (
            Statement[
                Var x (
                    Statement [
                        ValBind x false,
                        Conditional x (ValBind y l10) (ValBind y l20)
                    ]
                ),
                VarBind x y,
                ValBind x l10
            ]
        )
    )

proc0 = Proc [x,y,z] (Statement [ValBind x l1,VarBind y z,ValBind z l2])

prog5 = 
    Var x (
        Statement [
            ValBind x proc0,
            Nop
        ]
    )

proc1 = Proc [x] (VarBind x y)

prog6 = 
    Var y (
        Var a (
            Statement [
                Var y (
                    Statement [
                        ValBind y l10,
                        ValBind a proc1
                    ]
                ),
                Apply a [y],
                Nop
            ]
        )
    )

prog7 = 
    Var x (
        Var y (
            Var a (
                Statement [
                    ValBind y l100,
                    Var y (
                        Statement [
                            ValBind y l10,
                            ValBind a proc1
                        ]
                    ),
                    Apply a [x],
                    ValBind y l100,
                    Nop
                ]
            )
        )
    )

proc2 = Proc [x] $ Statement [ValBind x l10, ValBind y (Proc [c] Nop), Nop]

prog8 = 
    Var a (
        Var b (
            Var y (
                Statement [
                    ValBind a proc2,
                    Apply a [b],
                    Nop
                ]
            )
        )
    )

proc3 = Proc [x,y,z] (Statement [ValBind x l1,VarBind y z,ValBind z l2, VarBind a z])

prog9 = 
    Var a (
        Var b (
            Var c (
                Var d (
                    Var p (
                        Statement [
                            ValBind p proc3,
                            Apply p [b,c,d],
                            Nop
                        ]
                    )
                )
            )
        )
    )



























