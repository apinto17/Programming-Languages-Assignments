#lang typed/racket

(require typed/rackunit)

;; DXUQ4 Language

;; Numbers and Operations
(struct numC ([n : Real])
  #:transparent)
(struct ifC ([arg : DXUQ4]
             [t : DXUQ4]
             [f : DXUQ4])
  #:transparent)

;; Functions
(struct idC([s : Symbol])
  #:transparent)
(struct appC([body : DXUQ4]
             [args : (Listof DXUQ4)])
  #:transparent)
(struct lamC ([args : (Listof Symbol)]
              [body : DXUQ4])
  #:transparent)

(define-type DXUQ4 (U numC ifC appC idC lamC))

;; Values
(struct numV ([n : Real])
  #:transparent)
(struct boolV ([b : Boolean])
  #:transparent)
(struct cloV ([args : (Listof Symbol)]
              [body : DXUQ4]
              [env : (Listof Binding)])
  #:transparent)
(struct primV ([s : Symbol])
  #:transparent)

(define-type valC (U numV boolV cloV primV))

;; Environment
(struct Binding ([name : Symbol]
                  [val : valC])
  #:transparent)

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
(define (lookup [s : Symbol] [env : (Listof Binding)]) : valC
   (cond
    [(empty? env) (error "DXUQ4 Unbound identifier")]
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
    [(symbol=? 'fn s) (error "DXUQ4 Invalid identifier name")]
    [(symbol=? 'if s) (error "DXUQ4 Invalid identifier name")]
    [else (idC s)]))

(check-equal? (check-id-name 'x) (idC 'x))
(check-equal? (check-id-name 'something) (idC 'something))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (check-id-name 'fn)))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (check-id-name 'if)))

;; Ensure argument's name is unique 
(define (check-dup-symbol [in : (Listof Symbol)] [what : Symbol] [seen : Boolean]) : Boolean
  (cond
    [(empty? in) #t]
    [(equal? (first in) what) (if seen
                                  (error "DXUQ4 Duplicate identifier name")
                                  (check-dup-symbol (rest in) what #t))]
    [else (when (rest in) (check-dup-symbol (rest in) what seen))]))

(check-equal? (check-dup-symbol '() 'a #f) #t)
(check-equal? (check-dup-symbol '(a b c) 'a #f) #t)
(check-equal? (check-dup-symbol '(a b c d e f) 'a #f) #t)
(check-exn (regexp (regexp-quote "DXUQ4 Duplicate identifier name"))
           (lambda () (check-dup-symbol '(a) 'a #t)))
(check-exn (regexp (regexp-quote "DXUQ4 Duplicate identifier name"))
           (lambda () (check-dup-symbol '(a a) 'a #f)))
(check-exn (regexp (regexp-quote "DXUQ4 Duplicate identifier name"))
           (lambda () (check-dup-symbol '(a b c d a) 'a #f)))

;; Helper to check arg name and dup
(define (check-arguments [toCheck : (Listof Symbol)] [args : (Listof Symbol)]) : (Listof idC)
  (cond
    [(empty? toCheck) '()]
    [else (begin
            (check-dup-symbol args (first toCheck) #f) 
            (cons (check-id-name (first toCheck)) (check-arguments (rest toCheck) args)))]))

(check-equal? (check-arguments '() '()) '())
(check-equal? (check-arguments '(a) '(a)) (list (idC 'a)))
(check-equal? (check-arguments '(a b c) '(a b c)) (list (idC 'a) (idC 'b) (idC 'c)))
;(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
;           (lambda () (check-arguments '(a b /) '(a b /))))
(check-exn (regexp (regexp-quote "DXUQ4 Duplicate identifier name"))
           (lambda () (check-arguments '(a b b) '(a b b))))

;; Top call to validate list of args
(define (validate-arguments [args : (Listof Symbol)]) : (Listof idC)
  (cond
    [(empty? args) '()]
    [else (check-arguments args args)]))

(check-equal? (validate-arguments '()) '())
(check-equal? (validate-arguments '(a)) (list (idC 'a)))
(check-equal? (validate-arguments '(a b c)) (list (idC 'a) (idC 'b) (idC 'c)))
;(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
;           (lambda () (validate-arguments '(a b /))))
(check-exn (regexp (regexp-quote "DXUQ4 Duplicate identifier name"))
           (lambda () (validate-arguments '(a b b))))

;; Returns String representation of DXUQ4
(define (serialize [what : valC]) : String
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
(define (parse [s : Sexp]) : DXUQ4
  (match s
    [(? real?) (numC s)]
    [(? symbol?) (check-id-name (cast s Symbol))]
    [(list 'if a b c) (ifC (parse a) (parse b) (parse c))]
    [(list 'fn (list (? symbol? args) ...) b)
     (lamC (cast args (Listof Symbol)) (parse b))]
    [(list a b ...)
     (appC (parse a)
           (map (λ ([x : Sexp]) (parse x)) b))]
    [_ (error "DXUQ4 Not a DXUQ4 expression")]))

(check-equal? (parse '1) (numC 1))
(check-equal? (parse '(+ 1 2)) (appC (idC '+) (list (numC 1) (numC 2))))
(check-equal? (parse '(- 2 1)) (appC (idC '-) (list (numC 2) (numC 1))))
(check-equal? (parse '(* 1 2)) (appC (idC '*) (list (numC 1) (numC 2))))
(check-equal? (parse '(/ 2 4)) (appC (idC '/) (list (numC 2) (numC 4))))
(check-equal? (parse '(if (equal? 1 1) 2 3)) (ifC (appC (idC 'equal?) (list (numC 1) (numC 1))) (numC 2) (numC 3)))

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

(check-exn (regexp (regexp-quote "DXUQ4 Not a DXUQ4 expression"))
           (lambda () (parse '((((((())))))))))



;; Interpret DXUQ4 expressions
(define (interp [a : DXUQ4] [env : (Listof Binding)]) : valC
  (match a
    [(numC n) (numV n)]
    [(idC s) (lookup s env)]
    [(ifC a t f)
     (define temp (interp a env))
     (if (boolV? temp)
         (if (boolV-b temp) (interp t env) (interp f env))
         (error 'interp "DXUQ4 isn't a boolean value"))]
    [(appC f args) (interp-app a env)]
    [(lamC args b) (cloV args b env)]))


(define (interp-app [a : appC] [env : (Listof Binding)]) : valC
  (let ([body : valC (interp (appC-body a) env)])
    (match body
      [(? numV? body) body]
      [(? cloV? body) (interp (cloV-body body)
                           (if (= (length (cloV-args body)) (length (appC-args a)))
                           (append env (map (λ ([s : Symbol] [v : DXUQ4])
                                              (Binding s (interp v env)))
                                            (cloV-args body) (appC-args a)))
                           (error "DXUQ4 inconsistent number of args")))]
      [(? primV? body) (if (equal? (length (appC-args a)) 2)
                         (let* ([val1 : valC (interp (first (appC-args a)) env)]
                                [val2 : valC (interp (first (rest (appC-args a))) env)])
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
                             [else (error "DXUQ4 Couldn't apply primitive: arguments weren't numbers or booleans")]))
                         (error "DXUQ4 Couldn't apply primitive: incorrect number of arguments"))])))

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
      (list (numC 1) (numC 1))) (appC (idC '<=) (list (numC 1) (numC 1))))) (numC 2) (numC 3)) top-env) (numV 2))
(check-exn (regexp (regexp-quote "DXUQ4 Couldn't apply primitive: arguments weren't numbers or booleans"))
           (lambda () (interp (ifC (appC (idC 'equal?) (list (numC 3)
           (appC (idC '<=) (list (numC 1) (numC 1))))) (numC 2) (numC 3)) top-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Couldn't apply primitive: incorrect number of arguments"))
           (lambda () (interp (ifC (appC (idC 'equal?) (list (appC (idC '<=)
           (list (numC 1) (numC 1))) (appC (idC '<=) (list (numC 1) (numC 1)))
           (appC (idC '<=) (list (numC 1) (numC 1))))) (numC 2) (numC 3)) top-env)))
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
(check-equal? (interp (appC (lamC '(a) (appC (lamC '(a) (appC (idC 'a)
                                                              (list (numC 1))))
                                             (list (numC 2))))
                            (list (numC 3))) top-env) (numV 3))

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
(check-exn (regexp (regexp-quote "DXUQ4 inconsistent number of args"))
           (lambda () (top-interp '((fn () 9) 17))))



"DONE"