#lang typed/racket

(provide (all-defined-out))

(define-type index Nonnegative-Integer)
(define-type natural Positive-Integer)

(define-type (maybe a) (U False a))

(: nothing False)
(define nothing #f)

(: something? (All (a) ((maybe a) -> Boolean)))
(define (something? maybe-value)
  (not (not maybe-value)))

(: certainly (All (a) ((maybe a) a -> a)))
(define (certainly maybe-value default-value)
  (if (not maybe-value)
      default-value
      maybe-value))
