#lang typed/racket

(provide (all-defined-out))

(require (for-syntax racket/syntax
                     racket/list))

(define-syntax (define-contained-type stx)
  (syntax-case stx ()
    [(_ id type)
     (with-syntax ([type-list (format-id stx "~a-list" #'id)]
                   [type-set (format-id stx "~a-set" #'id)]
                   [type-vector (format-id stx "~a-vector" #'id)])
       #'(begin
           (provide type-list)
           (provide type-set)
           (provide type-vector)
           (define-type type-list (Listof type))
           (define-type type-set (Setof type))
           (define-type type-vector (Vectorof type))))]))

(define-contained-type integer Integer)

(define-type boolean-get (Integer -> Boolean))
(define-type boolean-set! (Integer Boolean -> Void))
(define-type integer-get (Integer -> Integer))
(define-type integer-set! (Integer Integer -> Void))
(define-type float-get (Integer -> Float))
(define-type float-set! (Integer Float -> Void))
