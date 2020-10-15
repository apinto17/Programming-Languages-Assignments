;; ALL DONE

#lang typed/racket

(require typed/rackunit)

;; DXUQ3 Language

;; Numbers and Operations
(struct numC ([n : Real])
  #:transparent)
(struct ifleq0 ([arg : DXUQ3]
                [t : DXUQ3]
                [f : DXUQ3])
  #:transparent)
(struct binop ([op : Symbol]
               [l : DXUQ3]
               [r : DXUQ3])
  #:transparent)

;; Functions
(struct idC([s : Symbol])
  #:transparent)
(struct appC([fun : idC]
             [args : (Listof DXUQ3)])
  #:transparent)
(struct FunDefC ([name : idC]
                 [args : (Listof idC)]
                 [body : DXUQ3])
  #:transparent)

(define-type DXUQ3 (U numC ifleq0 binop appC idC))

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
      (error "DXUQ3 Unsupported binary operation")))

(check-equal? (getBinOp '+) +)
(check-equal? (getBinOp '-) -)
(check-equal? (getBinOp '*) *)
(check-equal? (getBinOp '/) /)
(check-exn (regexp (regexp-quote "DXUQ3 Unsupported binary operation"))
           (lambda () (getBinOp '%)))
(check-exn (regexp (regexp-quote "DXUQ3 Unsupported binary operation"))
           (lambda () (getBinOp 'a)))

;; Gets function body from list of fds
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error "DXUQ3 Reference to undefined function")]
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
(check-exn (regexp (regexp-quote "DXUQ3 Reference to undefined function"))
           (lambda () (get-fundef 'a '())))
(check-exn (regexp (regexp-quote "DXUQ3 Reference to undefined function"))
           (lambda () (get-fundef 'a (list (FunDefC (idC 'b) (list (idC 'x) (idC 'a)) (numC 2))))))

;; Substitutes 'what' for 'in' if 'for' matches
(define (subst [what : DXUQ3] [for : Symbol] [in : DXUQ3]) : DXUQ3
  (match in
    [(numC n) in]
    [(binop op l r) (binop op (subst what for l) (subst what for r))]
    [(ifleq0 arg t f) (ifleq0 (subst what for arg) (subst what for t) (subst what for f))]
    [(idC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(appC f a) (appC f (map (λ ([x : DXUQ3]) (subst what for x)) a))]))

(check-equal? (subst (numC 2) 'x (numC 5)) (numC 5))
(check-equal? (subst (numC 2) 'x (idC 'x)) (numC 2))
(check-equal? (subst (numC 2) 'x (binop '+ (idC 'x) (numC 5))) (binop '+  (numC 2) (numC 5)))
(check-equal? (subst (numC 2) 'x (ifleq0 (numC 5) (idC 'x) (idC 'y))) (ifleq0 (numC 5) (numC 2) (idC 'y)))
(check-equal? (subst (numC 2) 'x (appC (idC 'something) (list (numC 2)))) (appC (idC 'something) (list (numC 2))))
(check-equal? (subst (numC 2) 'x (appC (idC 'something) (list (numC 2) (idC 'x))))
              (appC (idC 'something) (list (numC 2) (numC 2))))

;; Substitutes a list of whats for a list of symbols
(define (subst-list [whats : (Listof DXUQ3)] [for : (Listof Symbol)] [in : DXUQ3]) : DXUQ3
  (cond
    [(and (empty? whats) (empty? for)) in]
    [(and (empty? whats) (not (empty? for))) (error "DXUQ3 Wrong number of arguments")]
    [(and (empty? for) (not (empty? whats))) (error "DXUQ3 Wrong number of arguments")]
    [else (subst-list (rest whats) (rest for) (subst (first whats) (first for) in))]))

(check-equal? (subst-list '() '() (numC 3)) (numC 3))
(check-equal? (subst-list (list (numC 1)) '(a) (idC 'a)) (numC 1))
(check-equal? (subst-list (list (numC 2) (numC 3)) '(a b) (binop '+ (idC 'a) (idC 'b)))
              (binop '+ (numC 2) (numC 3)))
(check-equal? (subst-list (list (numC 2) (numC 3)) '(a b) (ifleq0 (numC 2) (idC 'a) (idC 'b)))
              (ifleq0 (numC 2) (numC 2) (numC 3)))
(check-exn (regexp (regexp-quote "DXUQ3 Wrong number of arguments"))
           (lambda () (subst-list (list (numC 3)) '(a b) (binop '+ (idC 'a) (idC 'b)))))
(check-exn (regexp (regexp-quote "DXUQ3 Wrong number of arguments"))
           (lambda () (subst-list (list (numC 2) (numC 3)) '(a) (binop '+ (idC 'a) (idC 'b)))))

;; Make sure id name is valid
(define (check-id-name [s : Symbol]) : idC
  (cond [(hash-has-key? binOps s) (error "DXUQ3 Invalid identifier name")]
        [(symbol=? 'fundef s) (error "DXUQ3 Invalid identifier name")]
        [(symbol=? 'ifleq0 s) (error "DXUQ3 Invalid identifier name")]
        [else (idC s)]))

(check-equal? (check-id-name 'x) (idC 'x))
(check-equal? (check-id-name 'something) (idC 'something))
(check-exn (regexp (regexp-quote "DXUQ3 Invalid identifier name"))
           (lambda () (check-id-name '+)))
(check-exn (regexp (regexp-quote "DXUQ3 Invalid identifier name"))
           (lambda () (check-id-name 'fundef)))
(check-exn (regexp (regexp-quote "DXUQ3 Invalid identifier name"))
           (lambda () (check-id-name 'ifleq0)))

;; Ensure argument's name is unique 
(define (check-dup-symbol [toCheck : (Listof Symbol)] [s : Symbol] [seen : Boolean]) : Void
  (cond
    [(equal? (first toCheck) s) (if seen
                                    (error "DXUQ3 Duplicate identifier name")
                                    (check-dup-symbol (rest toCheck) s #t))]
    [else (check-dup-symbol (rest toCheck) s seen)]))

;(check-equal? (check-dup-symbol))

;; Helper to check arg name and dup
(define (check-arguments [toCheck : (Listof Symbol)] [args : (Listof Symbol)]) : (Listof idC)
  (cond
    [(empty? toCheck) '()]
    [else (begin
            (check-dup-symbol args (first toCheck) #f) 
            (cons (check-id-name (first toCheck)) (check-arguments (rest toCheck) args)))]))

;(check-equal? (check-arguments '(a) '(a)) (list (idC 'a)))
;(check-equal? (check-arguments '(a b c) '(a b c)) (list (idC 'a) (idC 'b) (idC 'c)))
;(check-exn (regexp (regexp-quote "DXUQ3 Invalid identifier name"))
;           (lambda () (check-arguments '(a b /) '(a b /))))

;; Top call to validate list of args
(define (validate-arguments [args : (Listof Symbol)]) : (Listof idC)
  (if args
      (check-arguments args args)
      '()))

;; TODO REMOVE
(define (PLACEHOLDER [s : (Listof Symbol)]) : (Listof idC)
  (cond
    [(empty? s) '()]
    [else (cons (idC (first s)) (PLACEHOLDER (rest s)))]))


;; Checks for duplicate function names
(define (check-function-names [fds : (Listof FunDefC)] [f : FunDefC]) : (Listof FunDefC)
  (cond
    [(empty? fds) '()]
    [(equal? (idC-s (FunDefC-name (first fds))) (idC-s (FunDefC-name f))) (error "DXUQ3 Not a valid function")]
    [else  (cons (first fds) (check-function-names (rest fds) f))]))

;; Parse Sexp into DXUQ3 expression
(define (parse [s : Sexp]) : DXUQ3
  (match s
    [(? real?) (numC s)]
    [(? symbol?) (check-id-name (cast s Symbol))]
    [(list 'ifleq0 a b c) (ifleq0 (parse a) (parse b) (parse c))]
    [(list (? symbol? name) a ...)
     (if (hash-has-key? binOps name)
         (if (equal? (length a) 2)
             (binop name (parse (first a)) (parse (first (rest a))))
             (error "DXUQ3 Not a DXUQ3 expression"))
         (appC (check-id-name name) (map (λ ([x : Sexp]) (parse x)) a)))]
    [_ (error "DXUQ3 Not a DXUQ3 expression")]))

(check-equal? (parse '1) (numC 1))
(check-equal? (parse '(+ 1 2)) (binop '+ (numC 1) (numC 2)))
(check-equal? (parse '(- 2 1)) (binop '- (numC 2) (numC 1)))
(check-equal? (parse '(* 1 2)) (binop '* (numC 1) (numC 2)))
(check-equal? (parse '(/ 2 4)) (binop '/ (numC 2) (numC 4)))
(check-equal? (parse '(ifleq0 0 2 3)) (ifleq0 (numC 0) (numC 2) (numC 3)))

(check-equal? (parse '(+ (+ 2 1) 2))
              (binop '+ (binop '+ (numC 2) (numC 1)) (numC 2)))
(check-equal? (parse '(- 2 (- 2 1)))
              (binop '- (numC 2) (binop '- (numC 2) (numC 1))))
(check-equal? (parse '(* 1 (/ 4 2)))
              (binop '* (numC 1) (binop '/ (numC 4) (numC 2))))
(check-equal? (parse '(/ (- 4 0) 4))
              (binop '/ (binop '- (numC 4) (numC 0)) (numC 4)))
(check-equal? (parse '(ifleq0 (- 1 1) (* 1 1) 3))
              (ifleq0 (binop '- (numC 1) (numC 1))
                      (binop '* (numC 1) (numC 1)) (numC 3)))

(check-equal? (parse 'something) (idC 'something))
(check-equal? (parse '(+ something else)) (binop '+ (idC 'something) (idC 'else)))
(check-equal? (parse '(something 1)) (appC (idC 'something) (list (numC 1))))
(check-equal? (parse '(what (+ 2 3))) (appC (idC 'what) (list (binop '+ (numC 2) (numC 3)))))
(check-equal? (parse '(what one two)) (appC (idC 'what) (list (idC 'one) (idC 'two))))
(check-equal? (parse '(huh one two three four))
              (appC (idC 'huh) (list (idC 'one) (idC 'two) (idC 'three) (idC 'four))))

(check-exn (regexp (regexp-quote "DXUQ3 Not a DXUQ3 expression"))
           (lambda () (parse '(+ 1 'something 'else))))
(check-exn (regexp (regexp-quote "DXUQ3 Not a DXUQ3 expression"))
           (lambda () (parse '((((((())))))))))

;; Create FunDefC from Sexp
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'fundef (list (? symbol? n) ...) b)
     (let ([n : (Listof Symbol) (cast n (Listof Symbol))])
       (when (symbol=? (first n) 'main)
         (when (not (equal? (length n) 1))
           (error "DXUQ3 Main can't have an argument")))
       (FunDefC (check-id-name (first n)) (PLACEHOLDER (rest n)) (parse b)))]
    [_ (error "DXUQ3 Not a valid function")]))

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

(check-exn (regexp (regexp-quote "DXUQ3 Not a valid function"))
           (lambda () (parse-fundef '())))
(check-exn (regexp (regexp-quote "DXUQ3 Main can't have an argument"))
           (lambda () (parse-fundef '{fundef {main x} {+ x 1}})))

;; Get list of function defs from sexp
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (cond
    [(empty? s) '()]
    [(list? s) (cons (parse-fundef (first s))
                     (check-function-names (parse-prog (rest s)) (parse-fundef (first s))))]
    [else (error "DXUQ3 Not a valid function")]))

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

(check-exn (regexp (regexp-quote "DXUQ3 Not a valid function"))
           (lambda () (parse-prog '{{fundef {f x} 14} {fundef {f x} 14}})))
(check-exn (regexp (regexp-quote "DXUQ3 Not a valid function"))
           (lambda () (parse-prog 'something)))
(check-exn (regexp (regexp-quote "DXUQ3 Not a valid function"))
           (lambda () (parse-prog '{fundef {main} {+ x 14}})))
(check-exn (regexp (regexp-quote "DXUQ3 Main can't have an argument"))
           (lambda () (parse-prog '{{fundef {main x} {+ x 14}}})))

;; Interpret DXUQ3 expressions
(define (interp [a : DXUQ3] [fds : (Listof FunDefC)]) : Real
  (match a
    [(numC n) n]
    [(binop op l r) (if (equal? (getBinOp op) /)
                        (if (zero? (interp r fds))
                            (error "DXUQ3 Division by zero")
                            ((getBinOp op) (interp l fds) (interp r fds)))
                        ((getBinOp op) (interp l fds) (interp r fds)))]
    [(ifleq0 arg t f) (if (positive? (interp arg fds)) (interp f fds) (interp t fds))]
    [(appC f args) (let ([fd : FunDefC (get-fundef (idC-s f) fds)])
                     (interp (subst-list
                              (map (λ ([x : DXUQ3]) (numC (interp x fds))) args)
                              (map (λ ([x : idC]) (idC-s x)) (FunDefC-args fd))
                              (FunDefC-body fd))
                             fds))]
    [(idC s) (error "DXUQ3 Identifier in interpreter")]))

(check-equal? (interp (numC 4) '()) 4)
(check-equal? (interp (binop '+ (numC 2) (numC 3)) '()) 5)
(check-equal? (interp (binop '- (numC 2) (numC 3)) '()) -1)
(check-equal? (interp (binop '* (numC 2) (numC 3)) '()) 6)
(check-equal? (interp (binop '/ (numC 4) (numC 2)) '()) 2)
(check-equal? (interp (ifleq0 (numC 0) (numC 2) (numC 3)) '()) 2)
(check-equal? (interp (ifleq0 (numC 1) (numC 2) (numC 3)) '()) 3)

(check-equal? (interp (binop '* (numC -1) (binop '+ (numC 2) (numC 1))) '()) -3)
(check-equal? (interp (binop '+ (binop '+ (numC 2) (numC 1)) (numC 2)) '()) 5)
(check-equal? (interp (binop '- (numC 2) (binop '- (numC 2) (numC 1))) '()) 1)
(check-equal? (interp (binop '* (numC 1) (binop '/ (numC 4) (numC 2))) '()) 2)
(check-equal? (interp (binop '/ (binop '- (numC 4) (numC 0)) (numC 4)) '()) 1)
(check-equal? (interp (ifleq0 (binop '- (numC 1) (numC 1))
                              (binop '* (numC 1) (numC 1)) (numC 3)) '()) 1)

(check-equal? (interp (appC (idC 'name) (list (numC 2) (numC 1)))
                      (list (FunDefC (idC 'name) (list (idC 'x) (idC 'y)) (binop '+ (idC 'x) (idC 'y))))) 3)
(check-equal? (interp (appC (idC 'name) (list (numC 2)))
                      (list (FunDefC (idC 'name) (list (idC 'x)) (binop '+ (idC 'x) (idC 'x))))) 4)
(check-equal? (interp (appC (idC 'name) (list (binop '+ (numC 2) (numC 2))))
                      (list (FunDefC (idC 'name) (list (idC 'x)) (binop '+ (idC 'x) (idC 'x))))) 8)
(check-exn (regexp (regexp-quote "DXUQ3 Identifier in interpreter"))
           (lambda () (interp (idC 'something) '())))
(check-exn (regexp (regexp-quote "DXUQ3 Division by zero"))
           (lambda () (interp (binop '/ (numC 4) (numC 0)) '())))
(check-exn (regexp (regexp-quote "DXUQ3 Division by zero"))
           (lambda () (interp (binop '/ (numC 4) (binop '- (numC 4) (numC 4))) '())))

;; Interpret main
(define (interp-fns [fds : (Listof FunDefC)]) : Real
  (interp (appC (idC 'main) '()) fds))

(check-equal? (interp-fns (list (FunDefC (idC 'main) '() (numC 14)))) 14)
(check-equal? (interp-fns (list (FunDefC (idC 'main) '() (appC (idC 'f) (list (numC 2))))
                                (FunDefC (idC 'f) (list (idC 'x)) (binop '+ (idC 'x) (numC 2))))) 4)
(check-equal? (interp-fns (list (FunDefC (idC 'main) '() (appC (idC 'f) (list (numC 2) (numC 3))))
                                (FunDefC (idC 'f) (list (idC 'x) (idC 'y)) (binop '* (idC 'x) (idC 'y))))) 6)
(check-exn (regexp (regexp-quote "DXUQ3 Reference to undefined function"))
           (lambda () (interp-fns (list
                                   (FunDefC (idC 'a) (list (idC 'init)) (appC (idC 'f) (list (numC 2))))
                                   (FunDefC (idC 'f) (list (idC 'x)) (binop '+ (idC 'x) (numC 2)))))))

;; Parse and interpret DXUQ3-formatted Sexp
(define (top-interp [s : Sexp]) : Real
  (interp-fns (parse-prog s)))

(check-equal? (top-interp '{{fundef {main} {addtwo 0}}
                            {fundef {addtwo x} (+ {addone x} {addone x})}
                            {fundef {addone y} (+ y 1)}}) 2)
(check-equal? (top-interp '{{fundef {main} {something 0}}
                            {fundef {something x} (ifleq0 x {truthy x} {falsy x})}
                            {fundef {truthy y} (+ y 10)}
                            {fundef {falsy z} (+ z 50)}}) 10)
(check-equal? (top-interp '{{fundef {main} (ifleq0 -3 (+ 4 5) (+ 2 9))}}) 9)
(check-equal? (top-interp '{{fundef {main} {addtwo 1 1}}
                            {fundef {addtwo x y} (+ {addone x} {addone y})}
                            {fundef {addone y} (+ y 1)}}) 4)

(check-equal? (top-interp '{{fundef {main} (something)} {fundef {something} 11}}) 11)
(check-equal? (top-interp '{{fundef {main} (something 1 2 3)} {fundef {something a b c} (+ a (+ b c))}}) 6)


"DONE"