#lang scribble/manual

-------------LANGUAGE UPDATE-------------

;;> Testfail: while evaluating (top-interp (quote ((fn (empty) ((fn (cons) ((fn (empty?) ((fn (first) ((fn (rest) ((fn (Y) ((fn (length) ((fn (addup) (addup (cons 3 (cons 17 empty)))) (Y (fn (addup) (fn (l) (if (empty? l) 0 (+ (first l) (addup (rest l))))))))) (Y (fn (length) (fn (l) (if (empty? l) 0 (+ 1 (length (rest l))))))))) ((fn (x) (fn (y) (y (fn (z) (((x x) y) z))))) (fn (x) (fn (y) (y (fn (z) (((x x) y) z)))))))) (fn (l) (l false)))) (fn (l) (l true)))) (fn (l) (equal? l empty)))) (fn (a b) (fn (select) (if select a b))))) 13))):
;  DXUQ4 Couldn't apply primitive: arguments weren't numbers or booleans


;;> Testfail: while evaluating (begin (parse (quote (if (fn () (fn () ((fn () (let (a = (fn () (if (((let (/ = ((if (let (- = (let (+ = (if (if true (fn (true false null +) false) (fn (- * /) "Hello")) (null (fn (equal? <=) 0) ("World" (let (true = +) in (1 (if "" (let (false = "Hello") in (let (null = -1) in (if 2.2 (if (let in "") - ((((let in /) -22/7) "World") *)) (let in 0)))) (if (let in (fn (a) (if (if <= a -1) 1 "World"))) "Hello" equal?)) b)) c d) e f) g)) in h)) (* = i) in j) k l))) (equal? = m) (<= = n) in o))) p q))) (b = r) (c = s) (d = t) in u))))) v w))) #t):
;  DXUQ4 Not a DXUQ4 expression-


;;> Testfail: expected exception with message containing DXUQ on test expression: '(top-interp `(3 4 5))


  Expr	 	=	 	Num
 	 	|	 	id
 	 	|	 	String
 	 	|	 	{if Expr Expr Expr}
 	 	|	 	{let {id = Expr} ... in Expr}
 	 	|	 	{fn {id ...} Expr}
 	 	|	 	{Expr Expr ...}



-------------SMALL TASKS-------------
All done indication at the top
Re-organize code according to feedback given
    Move test cases to the end of file
    Important data definitions first, followed by important functions

Error primV
Let
Errors with serialization

