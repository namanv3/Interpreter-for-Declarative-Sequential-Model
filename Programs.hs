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