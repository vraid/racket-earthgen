#lang typed/racket

(provide (all-defined-out))

(define-type flonum Flonum)
(define-type natural Natural)
(define-type (maybe a) (U False a))

(: nothing False)
(define nothing #f)

(define nothing? not)

(: something? (All (a) ((maybe a) -> Boolean)))
(define (something? maybe-value)
  (not (not maybe-value)))

(: certainly (All (a) ((maybe a) a -> a)))
(define (certainly maybe-value default)
  (if (not maybe-value)
      default
      maybe-value))

(require (for-syntax racket/syntax
                     racket/list))

(define-syntax (define-contained-type stx)
  (syntax-case stx ()
    [(_ id type)
     (with-syntax ([type-list (format-id stx "~a-list" #'id)]
                   [type-set (format-id stx "~a-set" #'id)]
                   [type-vector (format-id stx "~a-vector" #'id)])
       #'(begin
           (provide id)
           (provide type-list)
           (provide type-set)
           (provide type-vector)
           (define-type id type)
           (define-type type-list (Listof type))
           (define-type type-set (Setof type))
           (define-type type-vector (Vectorof type))))]))

(define-contained-type integer Integer)
(define-contained-type index Integer)
(define-contained-type maybe-index (maybe index))
