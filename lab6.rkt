#lang typed/racket
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




(define (random-symbol) : Symbol
  (match (random 8)
    [0 'a]
    [1 'b]
    [2 'c]
    [3 'd]
    [4 'e]
    [5 'f]
    [6 'g]
    [7 'h]))





