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
(struct binop ([op : Symbol]
               [l : DXUQ4]
               [r : DXUQ4])
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

(define-type DXUQ4 (U numC ifC binop appC idC lamC))

;; Values
(struct numV ([n : Real])
  #:transparent)
(struct boolV ([b : Boolean])
  #:transparent)
(struct cloV ([args : (Listof Symbol)]
              [body : DXUQ4]
              [env : (Listof Binding)])
  #:transparent)

(define-type valC (U numV boolV cloV))

;; Environment
(struct Binding ([name : Symbol]
                  [val : valC])
  #:transparent)

(define mt-env empty)
(define extend-env cons)
(define top-env
  (list (Binding 'true (boolV #t))
        (Binding 'false (boolV #f))))

;; Given a op symbol, returns the op
(define binOps (make-immutable-hash
                (list (cons '+ +)
                      (cons '- -)
                      (cons '* *)
                      (cons '/ /))))

;; Given a binary symbol, returns the operator
(define (getBinOp [op : Symbol]) : (-> Real Real Real)
  (if (hash-has-key? binOps op)
      (hash-ref binOps op)
      (error "DXUQ4 Unsupported binary operation")))

(check-equal? (getBinOp '+) +)
(check-equal? (getBinOp '-) -)
(check-equal? (getBinOp '*) *)
(check-equal? (getBinOp '/) /)
(check-exn (regexp (regexp-quote "DXUQ4 Unsupported binary operation"))
           (lambda () (getBinOp '%)))
(check-exn (regexp (regexp-quote "DXUQ4 Unsupported binary operation"))
           (lambda () (getBinOp 'a)))

;; Get symbol from env
(define (lookup [s : Symbol] [env : (Listof Binding)]) : valC
  (cond
    [(empty? env) (error "DXUQ4 Unbound identifier")]
    [(equal? s (Binding-name (first env))) (Binding-val (first env))]
    [else (lookup s (rest env))]))

(check-equal? (lookup 'a (list (Binding 'a (numV 4)))) (numV 4))
(check-equal? (lookup 'b (list (Binding 'a (numV 4)) (Binding 'b (numV 5)))) (numV 5))
(check-exn (regexp (regexp-quote "DXUQ4 Unbound identifier"))
           (lambda () (lookup 'a '())))
(check-exn (regexp (regexp-quote "DXUQ4 Unbound identifier"))
           (lambda () (lookup 'a (list (Binding 'b (numV 4))))))

;; Make sure id name is valid
(define (check-id-name [s : Symbol]) : idC
  (cond [(hash-has-key? binOps s) (error "DXUQ4 Invalid identifier name")]
        [(symbol=? 'fn s) (error "DXUQ4 Invalid identifier name")]
        [(symbol=? 'if s) (error "DXUQ4 Invalid identifier name")]
        [else (idC s)]))

(check-equal? (check-id-name 'x) (idC 'x))
(check-equal? (check-id-name 'something) (idC 'something))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (check-id-name '+)))
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
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (check-arguments '(a b /) '(a b /))))
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
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (validate-arguments '(a b /))))
(check-exn (regexp (regexp-quote "DXUQ4 Duplicate identifier name"))
           (lambda () (validate-arguments '(a b b))))

;; Returns String representation of DXUQ4
(define (serialize [what : valC]) : String
  (match what
    [(numV n) (~v n)]
    [(boolV b) (~v b)]
    [(cloV a b e) "#<procedure>"]))

(check-equal? (serialize (numV 4)) "4")
(check-equal? (serialize (boolV #t)) "#t")
(check-equal? (serialize (cloV '() (numC 4) '())) "#<procedure>")

;; Parse Sexp into DXUQ4 expression
(define (parse [s : Sexp]) : DXUQ4
  (match s
    [(? real?) (numC s)]
    [(? symbol?) (check-id-name (cast s Symbol))]
    [(list 'if a b c) (ifC (parse a) (parse b) (parse c))]
    [(list 'fn (list (? symbol? args) ...) b)
     (lamC (cast args (Listof Symbol)) (parse b))]
    [(list (? symbol? name) a ...)
     (if (hash-has-key? binOps name)
         (if (equal? (length a) 2)
             (binop name (parse (first a)) (parse (first (rest a))))
             (error "DXUQ4 Not a DXUQ4 expression"))
         (appC (check-id-name name) (map (λ ([x : Sexp]) (parse x)) a)))]
    [_ (error "DXUQ4 Not a DXUQ4 expression")]))

(check-equal? (parse '1) (numC 1))
(check-equal? (parse '(+ 1 2)) (binop '+ (numC 1) (numC 2)))
(check-equal? (parse '(- 2 1)) (binop '- (numC 2) (numC 1)))
(check-equal? (parse '(* 1 2)) (binop '* (numC 1) (numC 2)))
(check-equal? (parse '(/ 2 4)) (binop '/ (numC 2) (numC 4)))
(check-equal? (parse '(if 0 2 3)) (ifC (numC 0) (numC 2) (numC 3)))

(check-equal? (parse '(+ (+ 2 1) 2))
              (binop '+ (binop '+ (numC 2) (numC 1)) (numC 2)))
(check-equal? (parse '(- 2 (- 2 1)))
              (binop '- (numC 2) (binop '- (numC 2) (numC 1))))
(check-equal? (parse '(* 1 (/ 4 2)))
              (binop '* (numC 1) (binop '/ (numC 4) (numC 2))))
(check-equal? (parse '(/ (- 4 0) 4))
              (binop '/ (binop '- (numC 4) (numC 0)) (numC 4)))
(check-equal? (parse '(if (- 1 1) (* 1 1) 3))
              (ifC (binop '- (numC 1) (numC 1))
                   (binop '* (numC 1) (numC 1)) (numC 3)))

(check-equal? (parse 'something) (idC 'something))
(check-equal? (parse '(+ something else)) (binop '+ (idC 'something) (idC 'else)))
(check-equal? (parse '(something 1)) (appC (idC 'something) (list (numC 1))))
(check-equal? (parse '(what (+ 2 3))) (appC (idC 'what) (list (binop '+ (numC 2) (numC 3)))))
(check-equal? (parse '(what one two)) (appC (idC 'what) (list (idC 'one) (idC 'two))))
(check-equal? (parse '(huh one two three four))
              (appC (idC 'huh) (list (idC 'one) (idC 'two) (idC 'three) (idC 'four))))

(check-equal? (parse '{fn {} 4}) (lamC '() (numC 4)))
(check-equal? (parse '{fn {x} {x}}) (lamC '(x) (appC (idC 'x) '())))
(check-equal? (parse '{fn {x y} {+ x y}}) (lamC '(x y) (binop '+ (idC 'x) (idC 'y))))

(check-exn (regexp (regexp-quote "DXUQ4 Not a DXUQ4 expression"))
           (lambda () (parse '(+ 1 'something 'else))))
(check-exn (regexp (regexp-quote "DXUQ4 Not a DXUQ4 expression"))
           (lambda () (parse '((((((())))))))))

;; Interpret DXUQ4 expressions
(define (interp [a : DXUQ4] [env : (Listof Binding)]) : valC
  (match a
    [(numC n) (numV n)]
    [(idC s) (lookup s env)]
    [(binop op l r) (interp-binop a env)]
    [(ifC arg t f) (interp-if a env)]
    [(appC f args) (interp-app a env)]
    [(lamC args b) (cloV args b env)]))

(define (interp-binop [a : binop] [env : (Listof Binding)]) : numV
  (if (and (numV? (interp (binop-l a) env)) (numV? (interp (binop-r a) env)))
      (if (and (equal? (getBinOp (binop-op a)) /)
               (zero? (numV-n (cast (interp (binop-r a) env) numV))))
          (error "DXUQ4 Division by zero")
          (numV ((getBinOp (binop-op a)) (numV-n (cast  (interp (binop-l a) env) numV))
                                         (numV-n (cast (interp (binop-r a) env) numV)))))
      (error "DXUQ4 binop doesn't have real operands")))

(define (interp-if [a : ifC] [env : (Listof Binding)]) : valC
  (if (and (numV? (interp (ifC-arg a) env)) (numV? (interp (ifC-t a) env)) (numV? (interp (ifC-f a) env)))
      (if (positive? (numV-n (cast (interp (ifC-arg a) env) numV)))
          (interp (ifC-f a) env)
          (interp (ifC-t a) env))
      (error "DXUQ4 'if' doesn't have real operands")))

(define (interp-app [a : appC] [env : (Listof Binding)]) : valC
  (let ([body : valC (interp (appC-body a) env)])
    (cond
      [(numV? body) body]
      [(cloV? body)(interp (cloV-body body)
                           (append env (map (λ ([s : Symbol] [v : DXUQ4])
                                              (Binding s (interp v env)))
                                            (cloV-args body) (appC-args a))))]
      [else (error "DXUQ4 Couldn't apply function")])))

(check-equal? (interp (numC 4) mt-env) (numV 4))
(check-equal? (interp (binop '+ (numC 2) (numC 3)) mt-env) (numV 5))
(check-equal? (interp (binop '- (numC 2) (numC 3)) mt-env) (numV -1))
(check-equal? (interp (binop '* (numC 2) (numC 3)) mt-env) (numV 6))
(check-equal? (interp (binop '/ (numC 4) (numC 2)) mt-env) (numV 2))
(check-equal? (interp (ifC (numC 0) (numC 2) (numC 3)) mt-env) (numV 2))
(check-equal? (interp (ifC (numC 1) (numC 2) (numC 3)) mt-env) (numV 3))

(check-equal? (interp (binop '* (numC -1) (binop '+ (numC 2) (numC 1))) mt-env) (numV -3))
(check-equal? (interp (binop '+ (binop '+ (numC 2) (numC 1)) (numC 2)) mt-env) (numV 5))
(check-equal? (interp (binop '- (numC 2) (binop '- (numC 2) (numC 1))) mt-env) (numV 1))
(check-equal? (interp (binop '* (numC 1) (binop '/ (numC 4) (numC 2))) mt-env) (numV 2))
(check-equal? (interp (binop '/ (binop '- (numC 4) (numC 0)) (numC 4)) mt-env) (numV 1))
(check-equal? (interp (ifC (binop '- (numC 1) (numC 1))
                           (binop '* (numC 1) (numC 1)) (numC 3)) mt-env) (numV 1))

(check-equal? (interp (appC (lamC '() (binop '+ (numC 2) (numC 1))) '()) mt-env) (numV 3))
(check-equal? (interp (appC (lamC '(a b) (appC (binop '+ (idC 'a) (idC 'b)) (list (idC 'a) (idC 'b))))
                            (list (numC 1) (numC 2))) mt-env) (numV 3))
(check-equal? (interp (appC (lamC '(a b) (appC (binop '+ (idC 'a) (idC 'b)) (list (idC 'a) (idC 'b))))
                            (list (binop '+ (numC 1) (numC 2)) (numC 2))) mt-env) (numV 5))
(check-equal? (interp (appC (lamC '(a) (appC (lamC '(a) (appC (idC 'a)
                                                              (list (numC 1))))
                                             (list (numC 2))))
                            (list (numC 3))) mt-env) (numV 3))

(check-exn (regexp (regexp-quote "DXUQ4 Unbound identifier"))
           (lambda () (interp (idC 'something) mt-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Division by zero"))
           (lambda () (interp (binop '/ (numC 4) (numC 0)) mt-env)))
(check-exn (regexp (regexp-quote "DXUQ4 Division by zero"))
           (lambda () (interp (binop '/ (numC 4) (binop '- (numC 4) (numC 4))) mt-env)))

;; Parse and interpret DXUQ4-formatted Sexp
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

;(check-equal? (top-interp '{{fundef {main} {addtwo 0}}
;                            {fundef {addtwo x} (+ {addone x} {addone x})}
;                            {fundef {addone y} (+ y 1)}}) "2")
;(check-equal? (top-interp '{{fundef {main} {something 0}}
;                            {fundef {something x} (ifC x {truthy x} {falsy x})}
;                            {fundef {truthy y} (+ y 10)}
;                            {fundef {falsy z} (+ z 50)}}) "10")
;(check-equal? (top-interp '{{fundef {main} (ifC -3 (+ 4 5) (+ 2 9))}}) "9")
;(check-equal? (top-interp '{{fundef {main} {addtwo 1 1}}
;                            {fundef {addtwo x y} (+ {addone x} {addone y})}
;                            {fundef {addone y} (+ y 1)}}) "4")

;(check-equal? (top-interp '{{fundef {main} (something)} {fundef {something} 11}}) "11")
;(check-equal? (top-interp '{{fundef {main} (something 1 2 3)} {fundef {something a b c} (+ a (+ b c))}}) "6")


"DONE"