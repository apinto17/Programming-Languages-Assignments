#lang typed/racket

(require typed/rackunit)

;; DXUQ4 Language

;; Numbers and Operations
(struct numC ([n : Real])
  #:transparent)
(struct ifC ([arg : ExprC]
             [t : ExprC]
             [f : ExprC])
  #:transparent)

;; Functions
(struct idC([s : Symbol])
  #:transparent)
(struct appC([body : ExprC]
             [args : (Listof ExprC)])
  #:transparent)
(struct lamC ([args : (Listof Symbol)]
              [body : ExprC])
  #:transparent)
(struct stringC ([s : String])
  #:transparent)

(define-type ExprC (U numC ifC appC idC lamC stringC))

;; Values
(struct numV ([n : Real])
  #:transparent)
(struct boolV ([b : Boolean])
  #:transparent)
(struct cloV ([args : (Listof Symbol)]
              [body : ExprC]
              [env : Env])
  #:transparent)
(struct primV ([fun : (-> (Listof Value) Value)])
  #:transparent)
(struct stringV ([s : String])
  #:transparent)

(define-type Value (U numV boolV cloV primV stringV))

;; Environment
(define (add [args : (Listof Value)]) : Value
  (match args
    [(list (? numV? a) (? numV? b)) (numV (+ (numV-n a) (numV-n b)))]
    [else (error '+ "DXUQ4 Invalid arguments passed to +")]))

(define (multiply [args : (Listof Value)]) : Value
  (match args
    [(list (? numV? a) (? numV? b)) (numV (* (numV-n a) (numV-n b)))]
    [else (error '* "DXUQ4 Invalid arguments passed to *")]))

(define (subtract [args : (Listof Value)]) : Value
  (match args
    [(list (? numV? a) (? numV? b)) (numV (- (numV-n a) (numV-n b)))]
    [else (error '- "DXUQ4 Invalid arguments passed to -")]))

(define (divide [args : (Listof Value)]) : Value
  (match args
    [(list (? numV? a) (? numV? b)) (if (= (numV-n b) 0)
                                        (error '/ "DXUQ4 Division by zero")
                                        (numV (/ (numV-n a) (numV-n b))))]
    [else (error '/ "DXUQ4 Invalid arguments passed to /")]))

(define (lte [args : (Listof Value)]) : Value
  (match args
    [(list (? numV? a) (? numV? b)) (boolV (<= (numV-n a) (numV-n b)))]
    [else (error '<= "DXUQ4 Invalid arguments passed to <=")]))

(define (myEqual [args : (Listof Value)]) : Value
  (match args
    [(list a b) (boolV (equal? a b))]
    [else (error 'equal? "DXUQ4 Invalid arguments passed to equal?")]))

(struct Binding ([name : Symbol]
                 [val : Value])
  #:transparent)

(define-type Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)                                 
(define top-env
  (list (Binding 'true (boolV #t))
        (Binding 'false (boolV #f))
        (Binding '+ (primV add))
        (Binding '* (primV multiply))
        (Binding '- (primV subtract))
        (Binding '/ (primV divide))
        (Binding '<= (primV lte))
        (Binding 'equal? (primV myEqual))))

;; Get symbol from env
(define (lookup [s : Symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error "DXUQ4 Unbound identifier:" s)]
    [(equal? s (Binding-name (first env))) (Binding-val (first env))]
    [else (lookup s (rest env))]))

(check-equal? (lookup 'a (list (Binding 'a (numV 4)))) (numV 4))
(check-equal? (lookup 'b (list (Binding 'a (numV 4)) (Binding 'b (numV 5)))) (numV 5))
(check-equal? (lookup 'true top-env) (boolV #t))
(check-equal? (lookup '+ top-env) (primV add))
(check-exn (regexp (regexp-quote "DXUQ4 Unbound identifier"))
           (lambda () (lookup 'a '())))
(check-exn (regexp (regexp-quote "DXUQ4 Unbound identifier"))
           (lambda () (lookup 'a (list (Binding 'b (numV 4))))))

;; Make sure id name is valid
(define (check-id-name [s : Symbol]) : idC
  (cond 
    [(symbol=? 'fn s) (error "DXUQ4 Invalid identifier name:" s)]
    [(symbol=? 'if s) (error "DXUQ4 Invalid identifier name:" s)]
    [(symbol=? 'in s) (error "DXUQ4 Invalid identifier name:" s)]
    [(symbol=? 'let s) (error "DXUQ4 Invalid identifier name:" s)]
    [else (idC s)]))

(check-equal? (check-id-name 'x) (idC 'x))
(check-equal? (check-id-name 'something) (idC 'something))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (check-id-name 'fn)))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (check-id-name 'if)))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (check-id-name 'in)))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (check-id-name 'let)))

;; Check the args for a lambda and make sure invalid symbols are rejected
(define (check-id-name-lam [lam-args : (Listof Symbol)]): Boolean
  (cond
    [(empty? lam-args) #t] 
    [(equal? (length lam-args) 1) (idC? (check-id-name (first lam-args)))]
    [else (and (check-id-name (first lam-args)) (check-id-name-lam (rest lam-args)))]))

(check-equal? (check-id-name-lam '(a b c)) #t)
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (check-id-name-lam '(a let c))))
(check-equal? (check-id-name-lam '(a a)) #t)

;; Ensure argument's name is unique 
(define (check-dup-symbol [in : (Listof Symbol)]) : Boolean
  (cond
    [(empty? in) #t]
    [else (if (> (length (filter (λ ([x : Symbol]) (equal? (first in) x)) in)) 1)
              #f #t)]))

(check-equal? (check-dup-symbol '()) #t)
(check-equal? (check-dup-symbol '(a b c)) #t)
(check-equal? (check-dup-symbol '(a b c d e f)) #t)
(check-equal? (check-dup-symbol '(a a)) #f)
(check-equal? (check-dup-symbol '(a b c d a)) #f)

;; Check validity of lamC's
(define (check-lam [lam : lamC]) : lamC
  (if (and (check-id-name-lam (lamC-args lam)) (check-dup-symbol (lamC-args lam)))
      lam
      (error "DXUQ4 Duplicate identifier name ~e" lam)))

(check-equal? (check-lam (lamC '() (numC 4))) (lamC '() (numC 4)))
(check-equal? (check-lam (lamC '(a) (numC 4))) (lamC '(a) (numC 4)))
(check-equal? (check-lam (lamC '(a b c) (numC 4))) (lamC '(a b c) (numC 4)))
(check-exn (regexp (regexp-quote "DXUQ4 Duplicate identifier name"))
           (lambda () (check-lam (lamC '(a b a) (numC 4)))))

;; Check validity of appC's 
(define (check-app [app : appC]) : appC
  app)

;; Returns String representation of DXUQ4
(define (serialize [what : Value]) : String
  (match what
    [(numV n) (~v n)]
    [(stringV s) s]
    [(boolV b) (if b "true" "false")]
    [(cloV a b e) "#<procedure>"]
    [(primV s) "#<primop>"]))

(check-equal? (serialize (numV 4)) "4")
(check-equal? (serialize (stringV "hello")) "hello")
(check-equal? (serialize (boolV #t)) "true")
(check-equal? (serialize (boolV #f)) "false")
(check-equal? (serialize (cloV '() (numC 4) '())) "#<procedure>")
(check-equal? (serialize (primV add)) "#<primop>")

;; Parse Sexp into DXUQ4 expression
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real?) (numC s)]
    [(? symbol?) (check-id-name (cast s Symbol))]
    [(? string?) (stringC s)]
    [(list 'if a b c) (ifC (parse a) (parse b) (parse c))]
    [(list 'fn (list (? symbol? args) ...) b)
     (check-lam (lamC (cast args (Listof Symbol)) (parse b)))]
    [(list 'let a ... 'in b)
     (match (cast a (Listof Sexp))
       [(list (list c '= d) ...) (appC (check-lam (lamC (cast c (Listof Symbol)) (parse b)))
                                       (map (λ (n) (parse n)) (cast d (Listof Sexp))))]
       [_ (error "DXUQ4 Not a valid let expression ~e" s)])]
    [(list a b ...)
     (check-app (appC (parse a) (map (λ ([x : Sexp]) (parse x)) b)))]
    [_ (error "DXUQ4 Not a DXUQ4 expression ~e" s)]))

(check-equal? (parse '1) (numC 1))
(check-equal? (parse '(+ 1 2)) (appC (idC '+) (list (numC 1) (numC 2))))
(check-equal? (parse '(- 2 1)) (appC (idC '-) (list (numC 2) (numC 1))))
(check-equal? (parse '(* 1 2)) (appC (idC '*) (list (numC 1) (numC 2))))
(check-equal? (parse '(/ 2 4)) (appC (idC '/) (list (numC 2) (numC 4))))
(check-equal? (parse '(if (equal? 1 1) 2 3))
              (ifC (appC (idC 'equal?) (list (numC 1) (numC 1))) (numC 2) (numC 3)))

(check-equal? (parse '(+ (+ 2 1) 2))
              (appC (idC '+) (list (appC (idC '+) (list (numC 2) (numC 1))) (numC 2))))
(check-equal? (parse '(- 2 (- 2 1)))
              (appC (idC '-) (list (numC 2) (appC (idC '-) (list (numC 2) (numC 1))))))
(check-equal? (parse '(* 1 (/ 4 2)))
              (appC (idC '*) (list (numC 1) (appC (idC '/) (list (numC 4) (numC 2))))))
(check-equal? (parse '(/ (- 4 0) 4))
              (appC (idC '/) (list (appC (idC '-) (list (numC 4) (numC 0))) (numC 4))))
(check-equal? (parse '(if (equal? 1 0) (* 1 1) 3))
              (ifC (appC (idC 'equal?) (list (numC 1) (numC 0)))
                   (appC (idC '*) (list (numC 1) (numC 1))) (numC 3)))

(check-equal? (parse 'something) (idC 'something))
(check-equal? (parse '(+ something else)) (appC (idC '+) (list (idC 'something) (idC 'else))))
(check-equal? (parse '(something 1)) (appC (idC 'something) (list (numC 1))))
(check-equal? (parse '(what (+ 2 3))) (appC (idC 'what) (list (appC (idC '+) (list (numC 2) (numC 3))))))
(check-equal? (parse '(what one two)) (appC (idC 'what) (list (idC 'one) (idC 'two))))
(check-equal? (parse '(huh one two three four))
              (appC (idC 'huh) (list (idC 'one) (idC 'two) (idC 'three) (idC 'four))))

(check-equal? (parse '{fn {} 4}) (lamC '() (numC 4)))
(check-equal? (parse '{fn {x} {x}}) (lamC '(x) (appC (idC 'x) '())))
(check-equal? (parse '{fn {x y} {+ x y}}) (lamC '(x y) (appC (idC '+) (list (idC 'x) (idC 'y)))))

(check-equal? (parse '{let {z = 0} in {z}}) (appC (lamC '(z) (appC (idC 'z) '())) (list (numC 0))))
(check-equal? (parse '{let {z = {+ 9 14}} {y = 98} in {+ z y}})
              (appC (lamC '(z y) (appC (idC '+) (list (idC 'z) (idC 'y))))
                    (list
                     (appC (idC '+) (list (numC 9) (numC 14)))
                     (numC 98))))
(check-exn (regexp (regexp-quote "DXUQ4 Duplicate identifier name"))
           (lambda () (parse '{let {z = 0} {z = 1} in {z}})))

(check-exn (regexp (regexp-quote "DXUQ4 Duplicate identifier name"))
           (lambda () (parse '(parse '(fn (x x) 3)))))
(check-exn (regexp (regexp-quote "DXUQ4 Not a DXUQ4 expression"))
           (lambda () (parse '((((((())))))))))
(check-exn (regexp (regexp-quote "DXUQ4 Not a valid let expression"))
           (lambda () (parse '(let ((fn = "")) in "World"))))

;; Interpret DXUQ4 expressions
(define (interp [a : ExprC] [env : Env]) : Value
  (match a
    [(numC n) (numV n)]
    [(idC s) (lookup s env)]
    [(stringC s) (stringV s)]
    [(ifC a t f)
     (define exp (interp a env))
     (if (boolV? exp)
         (if (boolV-b exp) (interp t env) (interp f env))
         (error 'interp "DXUQ4 isn't a boolean value ~e" exp))]
    [(appC f args) (match (interp f env)
                     [(cloV cloArgs cloBody cloEnv) (if (equal? (length args) (length cloArgs))
                                                        (interp cloBody
                                                                (append (map (λ ([s : Symbol] [v : ExprC])                                                               
                                                                                      (Binding s (interp v env)))
                                                                                    cloArgs args) cloEnv))
                                                        (error 'interp "DXUQ4 Inconsistent number of args"))]
                     [(primV f) (f (map (λ ([x : ExprC]) (interp x env)) args))]
                     [else (error "DXUQ4 Can't apply function" f)])]
    [(lamC args b) (cloV args b env)]))

(check-equal? (interp (stringC "hello") mt-env) (stringV "hello"))
(check-equal? (interp (numC 4) mt-env) (numV 4))
(check-equal? (interp (appC (idC '+) (list (numC 2) (numC 3))) top-env) (numV 5))
(check-equal? (interp (appC (idC '-) (list (numC 2) (numC 3))) top-env) (numV -1))
(check-equal? (interp (appC (idC '*) (list (numC 2) (numC 3))) top-env) (numV 6))
(check-equal? (interp (appC (idC '/) (list (numC 4) (numC 2))) top-env) (numV 2))
(check-equal? (interp (ifC (appC (idC 'equal?) (list (numC 1) (numC 1))) (numC 2) (numC 3)) top-env) (numV 2))
(check-equal? (interp (ifC (appC (idC '<=) (list (numC 1) (numC 1))) (numC 2) (numC 3)) top-env) (numV 2))
(check-equal? (interp (ifC (appC (idC '<=) (list (numC 1) (numC 0))) (numC 2) (numC 3)) top-env) (numV 3))
(check-equal? (interp (ifC (appC (idC 'equal?)
                                 (list (appC (idC '<=)
                                             (list (numC 1) (numC 1)))
                                       (appC (idC '<=) (list (numC 1) (numC 1)))))
                           (numC 2) (numC 3)) top-env) (numV 2))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid arguments passed to equal?"))
           (lambda () (interp (ifC (appC (idC 'equal?) (list (appC (idC '<=)
                                                                   (list (numC 1) (numC 1)))
                                                             (appC (idC '<=) (list (numC 1) (numC 1)))
                                                             (appC (idC '<=) (list (numC 1) (numC 1)))))
                                   (numC 2) (numC 3)) top-env)))
(check-exn (regexp (regexp-quote "DXUQ4 isn't a boolean value"))
           (lambda () (interp (ifC (numC 3) (numC 2) (numC 3)) top-env)))


(check-equal? (interp (appC (idC '*) (list (numC -1) (appC (idC '+) (list (numC 2) (numC 1))))) top-env) (numV -3))
(check-equal? (interp (appC (idC '+) (list (numC 2) (appC (idC '+) (list (numC 2) (numC 1))))) top-env) (numV 5))
(check-equal? (interp (appC (idC '-) (list (numC 2) (appC (idC '-) (list (numC 2) (numC 1))))) top-env) (numV 1))
(check-equal? (interp (appC (idC '*) (list (numC 1) (appC (idC '/) (list (numC 4) (numC 2))))) top-env) (numV 2))
(check-equal? (interp (appC (idC '/) (list (appC (idC '-) (list (numC 4) (numC 0))) (numC 4) )) top-env) (numV 1))
(check-equal? (interp (ifC (appC (idC 'equal?) (list (numC 1) (numC 1)))
                           (appC (idC '*) (list (numC 1) (numC 1)))
                           (numC 3)) top-env) (numV 1))

(check-equal? (interp (appC (lamC '() (appC (idC '+) (list (numC 2) (numC 1)))) '()) top-env) (numV 3))
(check-equal? (interp (appC (lamC '(a b) (appC (idC '+) (list (idC 'a) (idC 'b))))
                            (list (numC 1) (numC 2))) top-env) (numV 3))
(check-equal? (interp (appC (lamC '(a b) (appC (idC '+) (list (idC 'a) (idC 'b))))
                            (list (appC (idC '+) (list (numC 2) (numC 1))) (numC 2))) top-env) (numV 5))

(check-exn (regexp (regexp-quote "DXUQ4 Unbound identifier"))
           (lambda () (interp (idC 'something) mt-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Division by zero"))
           (lambda () (interp (appC (idC '/) (list (numC 2) (numC 0))) top-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Division by zero"))
           (lambda () (interp (appC (idC '/) (list (numC 4) (appC (idC '-) (list (numC 1) (numC 1))))) top-env)))

(check-exn (regexp (regexp-quote "DXUQ4 Invalid arguments passed to +"))
           (lambda () (interp (appC (idC '+) (list (numC 4) (numC 1) (numC 3))) top-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid arguments passed to -"))
           (lambda () (interp (appC (idC '-) (list (numC 4) (numC 1) (numC 3))) top-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid arguments passed to /"))
           (lambda () (interp (appC (idC '/) (list (numC 4) (numC 1) (numC 3))) top-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid arguments passed to *"))
           (lambda () (interp (appC (idC '*) (list (numC 4) (numC 1) (numC 3))) top-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid arguments passed to <="))
           (lambda () (interp (appC (idC '<=) (list (numC 4) (numC 1) (numC 3))) top-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid arguments passed to equal?"))
           (lambda () (interp (appC (idC 'equal?) (list (numC 4) (numC 1) (numC 3))) top-env)))

;; Parse and interpret DXUQ4-formatted Sexp
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

(check-equal? (top-interp '(if (equal? "hello" "hello") "world" "oops")) "world")
(check-equal? (top-interp '(if (equal? 1 0) (* 1 1) 3)) "3")
(check-equal? (top-interp '((fn (z y) (+ z y)) (+ 9 14) 98)) "121")
(check-equal? (top-interp (quote ((fn (+) (* + +)) 14))) "196")
(check-equal? (top-interp (quasiquote (if (<= 4 3) 29387 true))) "true")
(check-equal? (top-interp (quote (let (f = (fn (x) x)) in (let (y = 9) in (f 3))))) "3")
(check-equal? (top-interp (quote (let (z = 9) in (let (y = 9) (x = 5) in (+ z x))))) "14")
(check-equal? (top-interp (quote (let (z = (fn () 3)) (q = 9) in (+ (z) q)))) "12")
(check-equal? (top-interp (quote (let (f = (fn (a b c d e) (+ (+ a b) (+ (- 0 c) (+ d e))))) in (f 10 9 8 7 6)))) "24")
(check-equal? (top-interp (quote (let (+ = -) (- = +) in (+ 3 (- 6 4))))) "-7")
(check-equal? (top-interp (quote ((fn (seven) (seven))
                                  ((fn (minus) (fn () (minus (+ 3 10) (* 2 3)))) (fn (x y) (+ x (* -1 y))))))) "7")
(check-exn (regexp (regexp-quote "DXUQ4 Inconsistent number of args"))
           (lambda () (top-interp '((fn () 9) 17))))
(check-exn (regexp (regexp-quote "DXUQ4 Can't apply function"))
           (lambda () (top-interp '(((fn () 3)) 4 5))))

"DONE"