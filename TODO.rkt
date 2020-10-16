#lang scribble/manual

check-id update
    can't be let, in, if, or fn

-------------LANGUAGE UPDATE-------------
String data type
(let (id = expr) ... in Expr
(fn (id ...) expr)
(expr expr...)
Values
    Reals, Booleans, Strings, Closures, and Primitive ops
        PrimOps: + - * / <= equal?

Environment instead of subst
Serialization
    see ~v
No more function names

-------------SMALL TASKS-------------
Update interp to call interp-binop helper function
    Check is numbers are numV
All done indication at the top
Re-organize code according to feedback given
    Move test cases to the end of file
    Important data definitions first, followed by important functions
Update errors to display incorrect program text