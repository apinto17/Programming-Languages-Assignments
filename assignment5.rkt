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

(struct boxV ([l : Location]))

(struct nullV ()
  #:transparent)

(define-type valC (U numV boolV cloV primV boxV nullV))

;; Environment definitions
(define-type-alias Location Real)

(define-type-alias Store (HashTable Location valC))
(: store Store)
(define store (make-hash))

(define mt-store empty)
(define override-store cons)

(struct Binding ([name : Symbol]
                  [val : Location])
  #:transparent)

;; Initialize Environment with binops
(hash-set! store 0 (boolV #t))
(hash-set! store 1 (boolV #f))
(hash-set! store 2 (primV '+))
(hash-set! store 3 (primV '*))
(hash-set! store 4 (primV '-))
(hash-set! store 5 (primV '/))
(hash-set! store 6 (primV '<=))
(hash-set! store 7 (primV 'equal?))

(define mt-env empty)
(define extend-env cons)
(define top-env
  (list (Binding 'true 0)
        (Binding 'false 1)
        (Binding '+ 2)
        (Binding '* 3)
        (Binding '- 4)
        (Binding '/ 5)
        (Binding '<= 6)
        (Binding 'equal? 7)))



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
    [(primV s) "#<primop>"]
    [(nullV) "null"]))

(check-equal? (serialize (numV 4)) "4")
(check-equal? (serialize (boolV #t)) "#t")
(check-equal? (serialize (cloV '() (numC 4) '())) "#<procedure>")
(check-equal? (serialize (primV '+)) "#<primop>")
(check-equal? (serialize (nullV)) "null")




;; Interpret DXUQ4 expressions
(define (interp [a : DXUQ4] [env : (Listof Binding)] [store : Store]) : valC
  (match a
    [(numC n) (numV n)]
    [(idC s) (error "unimplemented")]
    [(ifC a t f) (error "unimplemented")]
    [(appC f args) (interp-app a env store)]
    [(lamC args b) (error "unimplemented")]))


(define (interp-app [a : appC] [env : (Listof Binding)] [store : Store]) : valC
  (let ([body : valC (interp (appC-body a) env store)])
    (match body
      [(? numV? body) body]
      [(? cloV? body) (interp (cloV-body body)
                           (if (= (length (cloV-args body)) (length (appC-args a)))
                           (append env (map (λ ([s : Symbol] [v : DXUQ4])
                                              (Binding s (interp v env store)))
                                            (cloV-args body) (appC-args a)))
                           (error "DXUQ4 inconsistent number of args")) store)]
      [(? primV? body) (if (equal? (length (appC-args a)) 2)
                         (let* ([val1 : valC (interp (first (appC-args a)) env store)]
                                [val2 : valC (interp (first (rest (appC-args a))) env store)])
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



(check-equal? (interp (appC (idC '*) (list (numC -1) (appC (idC '+) (list (numC 2) (numC 1))))) top-env store) (numV -3))
(check-equal? (interp (appC (idC '+) (list (numC 2) (appC (idC '+) (list (numC 2) (numC 1))))) top-env store) (numV 5))
(check-equal? (interp (appC (idC '-) (list (numC 2) (appC (idC '-) (list (numC 2) (numC 1))))) top-env store) (numV 1))
(check-equal? (interp (appC (idC '*) (list (numC 1) (appC (idC '/) (list (numC 4) (numC 2))))) top-env store) (numV 2))
(check-equal? (interp (appC (idC '/) (list (appC (idC '-) (list (numC 4) (numC 0))) (numC 4) )) top-env store) (numV 1))




"DONE"