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

(define-type ExprC (U numC ifC appC idC lamC))

;; Values
(struct numV ([n : Real])
  #:transparent)
(struct boolV ([b : Boolean])
  #:transparent)
(struct cloV ([args : (Listof Symbol)]
              [body : ExprC]
              [env : Env])
  #:transparent)
(struct primV ([s : Symbol])
  #:transparent)

(define-type Value (U numV boolV cloV primV))

;; Environment
(struct Binding ([name : Symbol]
                 [val : Value])
  #:transparent)

(define-type Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)
(define top-env
  (list (Binding 'true (boolV #t))
        (Binding 'false (boolV #f))
        (Binding '+ (primV '+))
        (Binding '* (primV '*))
        (Binding '- (primV '-))
        (Binding '/ (primV '/))
        (Binding '<= (primV '<=))
        (Binding 'equal? (primV 'equal?))))

;; Get symbol from env
(define (lookup [s : Symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error "DXUQ4 Unbound identifier:" s)]
    [(equal? s (Binding-name (first env))) (Binding-val (first env))]
    [else (lookup s (rest env))]))

(check-equal? (lookup 'a (list (Binding 'a (numV 4)))) (numV 4))
(check-equal? (lookup 'b (list (Binding 'a (numV 4)) (Binding 'b (numV 5)))) (numV 5))
(check-equal? (lookup 'true top-env) (boolV #t))
(check-equal? (lookup '+ top-env) (primV '+))
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
  (if (check-dup-symbol (lamC-args lam))
      lam
      (error "DXUQ4 Duplicate identifier name")))

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
    [(boolV b) (~v b)]
    [(cloV a b e) "#<procedure>"]
    [(primV s) "#<primop>"]))

(check-equal? (serialize (numV 4)) "4")
(check-equal? (serialize (boolV #t)) "#t")
(check-equal? (serialize (cloV '() (numC 4) '())) "#<procedure>")
(check-equal? (serialize (primV '+)) "#<primop>")

;; Parse Sexp into DXUQ4 expression
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real?) (numC s)]
    [(? symbol?) (check-id-name (cast s Symbol))]
    [(list 'if a b c) (ifC (parse a) (parse b) (parse c))]
    [(list 'fn (list (? symbol? args) ...) b)
     (check-lam (lamC (cast args (Listof Symbol)) (parse b)))]
    [(list 'let a ... 'in b)
     (match (cast a (Listof Sexp))
       [(list (list c '= d) ...) (appC (check-lam (lamC (cast c (Listof Symbol)) (parse b)))
                                       (map (λ (n) (parse n)) (cast d (Listof Sexp))))])]
    [(list a b ...)
     (check-app (appC (parse a) (map (λ ([x : Sexp]) (parse x)) b)))]
    [_ (error "DXUQ4 Not a DXUQ4 expression")]))

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

;; Interpret DXUQ4 expressions
(define (interp [a : ExprC] [env : Env]) : Value
  (match a
    [(numC n) (numV n)]
    [(idC s) (lookup s (reverse env))]
    [(ifC a t f)
     (define temp (interp a env))
     (if (boolV? temp)
         (if (boolV-b temp) (interp t env) (interp f env))
         (error 'interpp "DXUQ4 isn't a boolean value"))]
    [(appC f args) (let ([body : Value (interp (appC-body a)
                                         (match f
                                           [(lamC lam-args lam-body) (if (= (length lam-args)
                                                                            (length args))
                                                                         (append env (map (λ ([s : Symbol] [v : ExprC])                                                               
                                                                                             (Binding s (interp v env)))
                                                                                          lam-args args))
                                                                         (error "DXUQ4 inconsistent number of args"))]
                                           [_ env]))])               
                     (match body
                       [(? numV? body) body]
                       [(? cloV? body) (interp (cloV-body body)
                                               (append env (map (λ ([s : Symbol] [v : ExprC])                                                               
                                                                  (Binding s (interp v env)))
                                                                (cloV-args body) (appC-args a))))]
                       [(? primV? body) (if (equal? (length (appC-args a)) 2)
                                            (let* ([val1 : Value (interp (first (appC-args a)) env)]
                                                   [val2 : Value (interp (first (rest (appC-args a))) env)])
                                              (cond
                                                [(and (numV? val1) (numV? val2))
                                                 (match (primV-s body)
                                                   ['+ (numV (+ (numV-n val1) (numV-n val2)))]
                                                   ['- (numV (- (numV-n val1) (numV-n val2)))]
                                                   ['/ (numV (/ (numV-n val1) (if (zero? (numV-n val2))
                                                                                  (error "DXUQ4 Division by zero")
                                                                                  (numV-n val2))))]
                                                   ['* (numV (* (numV-n val1) (numV-n val2)))]
                                                   ['<= (boolV (<= (numV-n val1) (numV-n val2)))]
                                                   ['equal? (boolV (= (numV-n val1) (numV-n val2)))])]
                                                [(and (boolV? val1) (boolV? val2))
                                                 (match (primV-s body)
                                                   ['equal? (and val1 val2)])]
                                                [else (error "DXUQ4 Couldn't apply primitive (not num or bool)")]))
                                            (error "DXUQ4 Couldn't apply primitive: incorrect number of arguments"))]))]
    [(lamC args b) (cloV args b env)]))

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
(check-exn (regexp (regexp-quote "DXUQ4 Couldn't apply primitive (not num or bool)"))
           (lambda () (interp (ifC (appC (idC 'equal?) (list (numC 3)
                                                             (appC (idC '<=) (list (numC 1) (numC 1)))))
                                   (numC 2) (numC 3)) top-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Couldn't apply primitive: incorrect number of arguments"))
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
(check-equal? (interp (appC (lamC '(a b) (appC (appC (idC '+) (list (idC 'a) (idC 'b))) (list (idC 'a) (idC 'b))))
                            (list (numC 1) (numC 2))) top-env) (numV 3))
(check-equal? (interp (appC (lamC '(a b) (appC (appC (idC '+) (list (idC 'a) (idC 'b))) (list (idC 'a) (idC 'b))))
                            (list (appC (idC '+) (list (numC 2) (numC 1))) (numC 2))) top-env) (numV 5))

(check-exn (regexp (regexp-quote "DXUQ4 Unbound identifier"))
           (lambda () (interp (idC 'something) mt-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Division by zero"))
           (lambda () (interp (appC (idC '/) (list (numC 2) (numC 0))) top-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Division by zero"))
           (lambda () (interp (appC (idC '/) (list (numC 4) (appC (idC '-) (list (numC 1) (numC 1))))) top-env)))

;; Parse and interpret DXUQ4-formatted Sexp
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

(check-equal? (top-interp '(if (equal? 1 0) (* 1 1) 3)) "3")
(check-equal? (top-interp '((fn (z y) (+ z y)) (+ 9 14) 98)) "121")
(check-equal? (top-interp (quote ((fn (+) (* + +)) 14))) "196")
(check-exn (regexp (regexp-quote "DXUQ4 inconsistent number of args"))
           (lambda () (top-interp '((fn () 9) 17))))

;(parse (quote ((fn (seven) (seven))
;               ((fn (minus)
;                    (fn () (minus (+ 3 10) (* 2 3))))
;                (fn (x y) (+ x (* -1 y)))))))

;(check-equal? (top-interp (quote ((fn (seven) (seven))
;                                  ((fn (minus)
;                                       (fn () (minus (+ 3 10) (* 2 3))))
;                                   (fn (x y) (+ x (* -1 y))))))) "something")


"DONE"