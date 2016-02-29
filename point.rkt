#lang typed/racket

(provide (all-defined-out))

(struct point
  ([x : Integer]
   [y : Integer]))

(: point-equal? (point point -> Boolean))
(define (point-equal? p q)
  (and (equal? (point-x p) (point-x q))
       (equal? (point-y p) (point-y q))))

(: point-subtract (point point -> point))
(define (point-subtract p q)
  (point (- (point-x q) (point-x p))
         (- (point-y q) (point-y p))))

(define origin
  (point 0 0))
