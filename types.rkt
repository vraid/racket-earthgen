#lang typed/racket

(provide (all-defined-out))

(define-type index natural)
(define-type natural Nonnegative-Integer)
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
