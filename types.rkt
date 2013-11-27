#lang typed/racket

(provide index
         natural
         maybe
         nothing
         nothing?
         something?
         certainly)

(define-type index Nonnegative-Integer)
(define-type natural Positive-Integer)

(define-type (maybe a) (U False a))

(: nothing False)
(define nothing #f)

(: nothing? (All (a) ((maybe a) -> Boolean)))
(define (nothing? maybe-value)
  (false? maybe-value))

(: something? (All (a) ((maybe a) -> Boolean)))
(define (something? maybe-value)
  (not (nothing? maybe-value)))

(: certainly (All (a) ((maybe a) a -> a)))
(define (certainly maybe-value default-value)
  (if (false? maybe-value)
      default-value
      maybe-value))
