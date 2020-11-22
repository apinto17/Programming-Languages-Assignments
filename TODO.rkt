#lang scribble/manual

-------------LANGUAGE UPDATE-------------

(top-interp (quote ((fn (seven) (seven)) ((fn (minus) (fn () (minus (+ 3 10) (* 2 3)))) (fn (x y) (+ x (* -1 y))))))):
  DXUQ4 Unbound identifier: minus


;;> Testfail: while evaluating (top-interp (quote ((fn (seven) (seven)) ((fn (minus) (fn () (minus (+ 3 10) (* 2 3)))) (fn (x y) (+ x (* -1 y))))))):
;  DXUQ4 Unbound identifier

;;> Testfail: your code failed a test: (top-interp (quasiquote (if (<= 4 3) 29387 true))) evaluated to "#t", expecting "true"
;  DXUQ4 Couldn't apply primitive: arguments weren't numbers or booleans
;;> Testfail: expected exception with message containing DXUQ on test expression: '(top-interp `(3 4 5))
;;> Testfail: expected exception with message containing DXUQ on test expression: '(parse '(let (z = (fn () 3)) (z = 9) in (z)))
;;> Testfail: while evaluating (top-interp (quote (let (z = (fn () 3)) (q = 9) in (+ (z) q)))):
;  DXUQ4 Unbound identifier
;;> Testfail: while evaluating (top-interp (quote (let (f = (fn (a b c d e) (+ (+ a b) (+ (- 0 c) (+ d e))))) in (f 10 9 8 7 6)))):
;  DXUQ4 Unbound identifier
;;> Testfail: while evaluating (top-interp (quote (let (z = 9) in (let (y = 9) (x = 5) in (+ z x))))):
;  DXUQ4 Unbound identifier
;;> Testfail: while evaluating (top-interp (quote (let (f = (fn (x) x)) in (let (y = 9) in (f 3))))):
;  DXUQ4 Unbound identifier
;;> Testfail: expected exception with message containing DXUQ on test expression: '(top-interp '(((fn () 3)) 4 5))
;;> Testfail: while evaluating (top-interp (quote ((fn (empty) ((fn (cons) ((fn (empty?) ((fn (first) ((fn (rest) ((fn (Y) ((fn (length) ((fn (addup) (addup (cons 3 (cons 17 empty)))) (Y (fn (addup) (fn (l) (if (empty? l) 0 (+ (first l) (addup (rest l))))))))) (Y (fn (length) (fn (l) (if (empty? l) 0 (+ 1 (length (rest l))))))))) ((fn (x) (fn (y) (y (fn (z) (((x x) y) z))))) (fn (x) (fn (y) (y (fn (z) (((x x) y) z)))))))) (fn (l) (l false)))) (fn (l) (l true)))) (fn (l) (equal? l empty)))) (fn (a b) (fn (select) (if select a b))))) 13))):
;  DXUQ4 Couldn't apply primitive: arguments weren't numbers or booleans
;;> Testfail: while evaluating (top-interp (quote (let (+ = -) (- = +) in (+ 3 (- 6 4))))):
;  DXUQ4 Unbound identifier
;;> Testfail: while evaluating (begin (parse (quote (if (fn () (fn () ((fn () (let (a = (fn () (if (((let (/ = ((if (let (- = (let (+ = (if (if true (fn (true false null +) false) (fn (- * /) "Hello")) (null (fn (equal? <=) 0) ("World" (let (true = +) in (1 (if "" (let (false = "Hello") in (let (null = -1) in (if 2.2 (if (let in "") - ((((let in /) -22/7) "World") *)) (let in 0)))) (if (let in (fn (a) (if (if <= a -1) 1 "World"))) "Hello" equal?)) b)) c d) e f) g)) in h)) (* = i) in j) k l))) (equal? = m) (<= = n) in o))) p q))) (b = r) (c = s) (d = t) in u))))) v w))) #t):
;  DXUQ4 Not a DXUQ4 expression



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

