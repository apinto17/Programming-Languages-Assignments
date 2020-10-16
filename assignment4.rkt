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
(struct appC([fun : idC]
             [args : (Listof DXUQ4)])
  #:transparent)
(struct FunDefC ([name : idC]
                 [args : (Listof idC)]
                 [body : DXUQ4])
  #:transparent)

(define-type DXUQ4 (U numC ifC binop appC idC))

;; Environment
(struct BindingC ([name : Symbol]
                  [val : Real])
  #:transparent)
 
(define mt-env empty)
(define extend-env cons)

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

;; Gets function body from list of fds
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error "DXUQ4 Reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (idC-s (FunDefC-name (first fds)))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

(check-equal? (get-fundef 'a (list (FunDefC (idC 'a) (list (idC 'x)) (numC 2))))
              (FunDefC (idC 'a) (list (idC 'x)) (numC 2)))
(check-equal? (get-fundef 'b (list (FunDefC (idC 'a) (list (idC 'x)) (numC 2))
                                   (FunDefC (idC 'b) (list (idC 'y) (idC 'z)) (numC 3))))
              (FunDefC (idC 'b) (list (idC 'y) (idC 'z)) (numC 3)))
(check-equal? (get-fundef 'c (list (FunDefC (idC 'a) '() (numC 2))
                                   (FunDefC (idC 'b) (list (idC 'x)) (numC 3))
                                   (FunDefC (idC 'c) '() (numC 4))))
              (FunDefC (idC 'c) '() (numC 4)))
(check-exn (regexp (regexp-quote "DXUQ4 Reference to undefined function"))
           (lambda () (get-fundef 'a '())))
(check-exn (regexp (regexp-quote "DXUQ4 Reference to undefined function"))
           (lambda () (get-fundef 'a (list (FunDefC (idC 'b) (list (idC 'x) (idC 'a)) (numC 2))))))

;; Get symbol from env
(define (lookup [s : Symbol] [env : (Listof BindingC)]) : Real
  (cond
    [(empty? env) (error "DXUQ4 Unbound identifier")]
    [(equal? s (BindingC-name (first env))) (BindingC-val (first env))]
    [else (lookup s (rest env))]))

(check-equal? (lookup 'a (list (BindingC 'a 4))) 4)
(check-equal? (lookup 'b (list (BindingC 'a 4) (BindingC 'b 5))) 5)
(check-exn (regexp (regexp-quote "DXUQ4 Unbound identifier"))
           (lambda () (lookup 'a '())))
(check-exn (regexp (regexp-quote "DXUQ4 Unbound identifier"))
           (lambda () (lookup 'a (list (BindingC 'b 4)))))

;; Make sure id name is valid
(define (check-id-name [s : Symbol]) : idC
  (cond [(hash-has-key? binOps s) (error "DXUQ4 Invalid identifier name")]
        [(symbol=? 'fundef s) (error "DXUQ4 Invalid identifier name")]
        [(symbol=? 'ifC s) (error "DXUQ4 Invalid identifier name")]
        [else (idC s)]))

(check-equal? (check-id-name 'x) (idC 'x))
(check-equal? (check-id-name 'something) (idC 'something))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (check-id-name '+)))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (check-id-name 'fundef)))
(check-exn (regexp (regexp-quote "DXUQ4 Invalid identifier name"))
           (lambda () (check-id-name 'ifC)))

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

;; Checks for duplicate function names
(define (check-function-names [fds : (Listof FunDefC)] [f : FunDefC]) : (Listof FunDefC)
  (cond
    [(empty? fds) '()]
    [(equal? (idC-s (FunDefC-name (first fds))) (idC-s (FunDefC-name f))) (error "DXUQ4 Not a valid function")]
    [else  (cons (first fds) (check-function-names (rest fds) f))]))

;; Parse Sexp into DXUQ4 expression
(define (parse [s : Sexp]) : DXUQ4
  (match s
    [(? real?) (numC s)]
    [(? symbol?) (check-id-name (cast s Symbol))]
    [(list 'ifC a b c) (ifC (parse a) (parse b) (parse c))]
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
(check-equal? (parse '(ifC 0 2 3)) (ifC (numC 0) (numC 2) (numC 3)))

(check-equal? (parse '(+ (+ 2 1) 2))
              (binop '+ (binop '+ (numC 2) (numC 1)) (numC 2)))
(check-equal? (parse '(- 2 (- 2 1)))
              (binop '- (numC 2) (binop '- (numC 2) (numC 1))))
(check-equal? (parse '(* 1 (/ 4 2)))
              (binop '* (numC 1) (binop '/ (numC 4) (numC 2))))
(check-equal? (parse '(/ (- 4 0) 4))
              (binop '/ (binop '- (numC 4) (numC 0)) (numC 4)))
(check-equal? (parse '(ifC (- 1 1) (* 1 1) 3))
              (ifC (binop '- (numC 1) (numC 1))
                   (binop '* (numC 1) (numC 1)) (numC 3)))

(check-equal? (parse 'something) (idC 'something))
(check-equal? (parse '(+ something else)) (binop '+ (idC 'something) (idC 'else)))
(check-equal? (parse '(something 1)) (appC (idC 'something) (list (numC 1))))
(check-equal? (parse '(what (+ 2 3))) (appC (idC 'what) (list (binop '+ (numC 2) (numC 3)))))
(check-equal? (parse '(what one two)) (appC (idC 'what) (list (idC 'one) (idC 'two))))
(check-equal? (parse '(huh one two three four))
              (appC (idC 'huh) (list (idC 'one) (idC 'two) (idC 'three) (idC 'four))))

(check-exn (regexp (regexp-quote "DXUQ4 Not a DXUQ4 expression"))
           (lambda () (parse '(+ 1 'something 'else))))
(check-exn (regexp (regexp-quote "DXUQ4 Not a DXUQ4 expression"))
           (lambda () (parse '((((((())))))))))

;; Create FunDefC from Sexp
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'fundef (list (? symbol? n) ...) b)
     (let ([n : (Listof Symbol) (cast n (Listof Symbol))])
       (when (symbol=? (first n) 'main)
         (when (not (equal? (length n) 1))
           (error "DXUQ4 Main can't have an argument")))
       (FunDefC (check-id-name (first n)) (validate-arguments (rest n)) (parse b)))]
    [_ (error "DXUQ4 Not a valid function")]))

(check-equal? (parse-fundef '{fundef {main} 1})
              (FunDefC (idC 'main) '() (numC 1)))
(check-equal? (parse-fundef '{fundef {main} (func 1 2 3)})
              (FunDefC (idC 'main) '() (appC (idC 'func) (list (numC 1) (numC 2) (numC 3)))))
(check-equal? (parse-fundef '{fundef {name x} 1})
              (FunDefC (idC 'name) (list (idC 'x)) (numC 1)))
(check-equal? (parse-fundef '{fundef {addone x y} {+ x y}})
              (FunDefC (idC 'addone) (list (idC 'x) (idC 'y)) (binop '+ (idC 'x) (idC 'y))))
(check-equal? (parse-fundef '{fundef {addone x y z} {+ x (* 3 x)}})
              (FunDefC (idC 'addone) (list (idC 'x) (idC 'y) (idC 'z))
                       (binop '+ (idC 'x) (binop '* (numC 3) (idC 'x)))))

(check-exn (regexp (regexp-quote "DXUQ4 Not a valid function"))
           (lambda () (parse-fundef '())))
(check-exn (regexp (regexp-quote "DXUQ4 Main can't have an argument"))
           (lambda () (parse-fundef '{fundef {main x} {+ x 1}})))

;; Get list of function defs from sexp
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (cond
    [(empty? s) '()]
    [(list? s) (cons (parse-fundef (first s))
                     (check-function-names (parse-prog (rest s)) (parse-fundef (first s))))]
    [else (error "DXUQ4 Not a valid function")]))

(check-equal? (parse-prog '()) '())
(check-equal? (parse-prog '{{fundef {f x} 14}})
              (list (FunDefC (idC 'f) (list (idC 'x)) (numC 14))))
(check-equal? (parse-prog '{{fundef {f x y z} 14}})
              (list (FunDefC (idC 'f) (list (idC 'x) (idC 'y) (idC 'z)) (numC 14))))
(check-equal? (parse-prog '{{fundef {f x} {+ x 14}}})
              (list (FunDefC (idC 'f) (list (idC 'x)) (binop '+ (idC 'x) (numC 14)))))
(check-equal? (parse-prog '{{fundef {f x y} {+ x 14}} {fundef {main} {f 2}}})
              (list (FunDefC (idC 'f) (list (idC 'x) (idC 'y)) (binop '+ (idC 'x) (numC 14)))
                    (FunDefC (idC 'main) '() (appC (idC 'f) (list (numC 2))))))

(check-exn (regexp (regexp-quote "DXUQ4 Not a valid function"))
           (lambda () (parse-prog '{{fundef {f x} 14} {fundef {f x} 14}})))
(check-exn (regexp (regexp-quote "DXUQ4 Not a valid function"))
           (lambda () (parse-prog 'something)))
(check-exn (regexp (regexp-quote "DXUQ4 Not a valid function"))
           (lambda () (parse-prog '{fundef {main} {+ x 14}})))
(check-exn (regexp (regexp-quote "DXUQ4 Main can't have an argument"))
           (lambda () (parse-prog '{{fundef {main x} {+ x 14}}})))

;; Interpret DXUQ4 expressions
(define (interp [a : DXUQ4] [env : (Listof BindingC)] [fds : (Listof FunDefC)]) : Real
  (match a
    [(numC n) n]
    [(binop op l r) (if (equal? (getBinOp op) /)
                        (if (zero? (interp r env fds))
                            (error "DXUQ4 Division by zero")
                            ((getBinOp op) (interp l env fds) (interp r env fds)))
                        ((getBinOp op) (interp l env fds) (interp r env fds)))]
    [(ifC arg t f) (if (positive? (interp arg env fds))
                       (interp f env fds)
                       (interp t env fds))]
    [(appC f args) (let ([fd : FunDefC (get-fundef (idC-s f) fds)])
                     (interp (FunDefC-body fd) (bind-arguments (FunDefC-args fd) (interp-args args env fds)) fds))]
    [(idC s) (lookup s env)]))

;; Interpret list of DXUQ4 Expressions
(define (interp-args [args : (Listof DXUQ4)] [env : (Listof BindingC)] [fds : (Listof FunDefC)]) : (Listof Real)
  (cond
    [(empty? args) '()]
    [else (cons (interp (first args) env fds) (interp-args (rest args) env fds))]))

;; Bind arguments in the environment
(define (bind-arguments [args : (Listof idC)] [vals : (Listof Real)]) : (Listof BindingC)
  (cond
    [(empty? args) '()]
    [else (extend-env (BindingC (idC-s (first args)) (first vals)) (bind-arguments (rest args) (rest vals)))]))

(check-equal? (interp (numC 4) mt-env '()) 4)
(check-equal? (interp (binop '+ (numC 2) (numC 3)) mt-env '()) 5)
(check-equal? (interp (binop '- (numC 2) (numC 3)) mt-env '()) -1)
(check-equal? (interp (binop '* (numC 2) (numC 3)) mt-env '()) 6)
(check-equal? (interp (binop '/ (numC 4) (numC 2)) mt-env '()) 2)
(check-equal? (interp (ifC (numC 0) (numC 2) (numC 3)) mt-env '()) 2)
(check-equal? (interp (ifC (numC 1) (numC 2) (numC 3)) mt-env '()) 3)

(check-equal? (interp (binop '* (numC -1) (binop '+ (numC 2) (numC 1))) mt-env '()) -3)
(check-equal? (interp (binop '+ (binop '+ (numC 2) (numC 1)) (numC 2)) mt-env '()) 5)
(check-equal? (interp (binop '- (numC 2) (binop '- (numC 2) (numC 1))) mt-env '()) 1)
(check-equal? (interp (binop '* (numC 1) (binop '/ (numC 4) (numC 2))) mt-env '()) 2)
(check-equal? (interp (binop '/ (binop '- (numC 4) (numC 0)) (numC 4)) mt-env '()) 1)
(check-equal? (interp (ifC (binop '- (numC 1) (numC 1))
                           (binop '* (numC 1) (numC 1)) (numC 3)) mt-env '()) 1)

(check-equal? (interp (appC (idC 'name) (list (numC 2) (numC 1))) mt-env
                      (list (FunDefC (idC 'name) (list (idC 'x) (idC 'y)) (binop '+ (idC 'x) (idC 'y))))) 3)
(check-equal? (interp (appC (idC 'name) (list (numC 2))) mt-env
                      (list (FunDefC (idC 'name) (list (idC 'x)) (binop '+ (idC 'x) (idC 'x))))) 4)
(check-equal? (interp (appC (idC 'name) (list (binop '+ (numC 2) (numC 2)))) mt-env
                      (list (FunDefC (idC 'name) (list (idC 'x)) (binop '+ (idC 'x) (idC 'x))))) 8)
(check-exn (regexp (regexp-quote "DXUQ4 Unbound identifier"))
           (lambda () (interp (idC 'something) mt-env '())))
(check-exn (regexp (regexp-quote "DXUQ4 Division by zero"))
           (lambda () (interp (binop '/ (numC 4) (numC 0)) mt-env '())))
(check-exn (regexp (regexp-quote "DXUQ4 Division by zero"))
           (lambda () (interp (binop '/ (numC 4) (binop '- (numC 4) (numC 4))) mt-env '())))

;; Interpret main
(define (interp-fns [fds : (Listof FunDefC)]) : Real
  (interp (appC (idC 'main) '()) mt-env fds))

(check-equal? (interp-fns (list (FunDefC (idC 'main) '() (numC 14)))) 14)
(check-equal? (interp-fns (list (FunDefC (idC 'main) '() (appC (idC 'f) (list (numC 2))))
                                (FunDefC (idC 'f) (list (idC 'x)) (binop '+ (idC 'x) (numC 2))))) 4)
(check-equal? (interp-fns (list (FunDefC (idC 'main) '() (appC (idC 'f) (list (numC 2) (numC 3))))
                                (FunDefC (idC 'f) (list (idC 'x) (idC 'y)) (binop '* (idC 'x) (idC 'y))))) 6)
(check-exn (regexp (regexp-quote "DXUQ4 Reference to undefined function"))
           (lambda () (interp-fns (list
                                   (FunDefC (idC 'a) (list (idC 'init)) (appC (idC 'f) (list (numC 2))))
                                   (FunDefC (idC 'f) (list (idC 'x)) (binop '+ (idC 'x) (numC 2)))))))

;; Parse and interpret DXUQ4-formatted Sexp
(define (top-interp [s : Sexp]) : Real
  (interp-fns (parse-prog s)))

(check-equal? (top-interp '{{fundef {main} {addtwo 0}}
                            {fundef {addtwo x} (+ {addone x} {addone x})}
                            {fundef {addone y} (+ y 1)}}) 2)
(check-equal? (top-interp '{{fundef {main} {something 0}}
                            {fundef {something x} (ifC x {truthy x} {falsy x})}
                            {fundef {truthy y} (+ y 10)}
                            {fundef {falsy z} (+ z 50)}}) 10)
(check-equal? (top-interp '{{fundef {main} (ifC -3 (+ 4 5) (+ 2 9))}}) 9)
(check-equal? (top-interp '{{fundef {main} {addtwo 1 1}}
                            {fundef {addtwo x y} (+ {addone x} {addone y})}
                            {fundef {addone y} (+ y 1)}}) 4)

(check-equal? (top-interp '{{fundef {main} (something)} {fundef {something} 11}}) 11)
(check-equal? (top-interp '{{fundef {main} (something 1 2 3)} {fundef {something a b c} (+ a (+ b c))}}) 6)


"DONE"