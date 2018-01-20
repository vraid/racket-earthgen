#lang typed/racket

(provide (all-defined-out))

(require "constants.rkt")

(: nonzero? (Number -> Boolean))
(define (nonzero? a)
  (not (zero? a)))

(: relative-difference (Float Float -> Float))
(define (relative-difference a b)
  (cond
    [(= a b) 0.0]
    [(or (zero? a) (zero? b)) +inf.0]
    [else (+ -1.0
             (if (> a b)
                 (/ a b)
                 (/ b a)))]))

(define-type maybe-minmax ((Option Real) -> (Real -> Real)))
(: maybe-max maybe-minmax)
(define ((maybe-max low) num)
  (if low
      (max low num)
      num))

(: maybe-min maybe-minmax)
(define ((maybe-min high) num)
  (if high
      (min high num)
      num))

(: within-interval ((Option Real) (Option Real) -> (Real -> Real)))
(define ((within-interval low high) num)
  ((maybe-min high)
   ((maybe-max low) num)))

(: ratio-within (Float Float -> (Float -> Float)))
(define ((ratio-within low high) num)
  (define closest ((within-interval low high) num))
  (/ (- closest low)
     high))

(: index-within-range? (Integer Integer -> (Integer -> Boolean)))
(define ((index-within-range? low high) i)
  (and (>= i low) (< i high)))

(: angle-distance (Float Float -> Float))
(define (angle-distance a b)
  (abs
   (- (abs (- a b))
      tau)))
