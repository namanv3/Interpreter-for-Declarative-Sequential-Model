import DataTypes
import Sequential
import Concurrent

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
l5 = NumLiteral 5

l10  = NumLiteral 10
l20  = NumLiteral 20
l30  = NumLiteral 30
l40  = NumLiteral 40
l50  = NumLiteral 50
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
                ValBind x l20
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


rec1 = Record l1 [(l10,a), (l20,b)]
rec2 = Record l1 [(l10,y), (l20,a)]
rec3 = Record l1 [(l20,y), (l30,a)]
rec4 = Record l1 [(l10,y), (l20,z)]

prog10 = 
    Var a (
        Var b (
            Var x (
                Var y (
                    Statement [
                        ValBind x rec1,
                        VarBind a y,
                        Var z (
                            Statement [
                                ValBind z rec2,
                                VarBind x z
                            ]
                        )
                    ]
                )
            )
        )
    )

prog11 = 
    Var a (
        Var b (
            Var x (
                Var y (
                    Statement [
                        ValBind x rec1,
                        VarBind a y,
                        Var z (
                            Statement [
                                ValBind z rec3,
                                VarBind x z
                            ]
                        )
                    ]
                )
            )
        )
    )

prog12 = 
    Var a (
        Var b (
            Var x (
                Statement [
                    ValBind x rec1,
                    Match x rec4 (ValBind y l0) (ValBind b l1)
                ]
            )
        )
    )

proc4 = Proc [x] $ Var y (
                    Statement [
                        ValBind y l0,
                        Match x rec1 (ValBind a l1) Nop,
                        Match x rec1 (VarBind b y) Nop
                    ]
                )

prog13 = 
    Var a (
        Statement [
            ValBind a proc4,
            Var x (
                Var y (
                    Var z (
                        Statement [
                            ValBind x rec4,
                            Apply a [x],
                            Nop           
                        ]
                    )
                )
            )
        ]
    )

prog14 =
    Var a (
        Var b (
            Var c (
                Statement [
                    ValBind a l100,
                    ValBind b l200,
                    Product a b c,
                    Nop
                ]
            )
        )
    )

square = Proc [a,b] $ Product a a b

hypotenuse = Proc [a,b,c] $ Var x (
                                Var y (
                                    Var s (
                                        Statement [
                                            ValBind s square,
                                            Apply s [a,x],
                                            Apply s [b,y],
                                            Sum x y c
                                        ]
                                    )
                                )
                            )

prog15 = 
    Var x (
        Var y (
            Var z (
                Var r (
                    Statement [
                        ValBind x l10,
                        ValBind y l4,
                        ValBind r hypotenuse,
                        Apply r [x,y,z],
                        Nop
                    ]
                )
            )
        )
    )

prog16 =
    Var x (
        Statement [
            ValBind x l0,
            Conditional x Nop Nop
        ]
    )

prog17 = 
    Var x (
        Var y (
            Statement [
                Thread Nop,
                Thread (ValBind y l10),
                ValBind x l10
            ]
        )
    )

prog18 = 
    Var x (
        Var y (
            Var z (
                Var s (
                    Statement [
                        Thread (Apply s [x,y]),
                        ValBind x l20,
                        ValBind s square
                    ]
                )
            )
        )
    )

prog19 = 
    Var x (
        Var y (
            Var z (
                Var s (
                    Statement [
                        Thread (Apply s [x,y]),
                        Thread (Conditional z (ValBind x l20) (ValBind y l30)),
                        ValBind x l20,
                        ValBind s square,
                        ValBind z true
                    ]
                )
            )
        )
    )

prog20 = 
    Var x (
        Statement [
            Thread (ValBind x l10),
            ValBind x l20
        ]
    )


s1 = Ident "s1"
s2 = Ident "s2"
one = Ident "one"
two = Ident "two"
three = Ident "three"

prog21 = 
    Var one ( Var two (Var three (Var s1 ( Var s2 ( Var a ( Statement [
        ValBind one l1,
        ValBind two l2,
        ValBind three l3,
        ValBind a l10,
        Thread (
            Var b (
                Statement [
                    Sum a one b,
                    Conditional s1 Nop (Statement [Nop,Nop])
                ]
            )
        ),
        Thread (
            Var c (
                Statement [
                    Sum a two c,
                    Conditional s2 Nop (Statement [Nop,Nop]),
                    ValBind s1 true
                ]
            )
        ),
        Var a (
            Statement [
                ValBind a l100,
                Thread (
                    Var d (
                        Statement [
                            Sum a three d,
                            ValBind s2 false
                        ]
                    )
                )
            ]
        )
    ]))))))









