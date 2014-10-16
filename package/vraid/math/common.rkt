#lang typed/racket

(provide (all-defined-out)
         (all-from-out "constants.rkt"))

(require "constants.rkt"
         "../types.rkt")

(: negative (case-> (Flonum -> Flonum)
                    (Integer -> Integer)
                    (Number -> Number)))
(define (negative a)
  (- a))

(: divide (case-> (Flonum Flonum -> Flonum)
                  (Number Zero -> Zero)
                  (Integer Integer -> Exact-Rational)
                  (Number Number -> Number)))
(define (divide a b)
  (/ b a))

(: subtract (case-> (Integer Integer -> Integer)
                    (Flonum Flonum -> Flonum)
                    (Number Number -> Number)))
(define (subtract a b)
  (- b a))

(define sum +)

(define product *)

(define-type maybe-minmax ((maybe Real) -> (Real -> Real)))
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

(: within-interval ((maybe Real) (maybe Real) -> (Real -> Real)))
(define ((within-interval low high) num)
  ((maybe-min high)
   ((maybe-max low) num)))

(: ratio-within (Flonum Flonum -> (Flonum -> Flonum)))
(define ((ratio-within low high) num)
  (define closest ((within-interval low high) num))
  (/ (- closest low)
     high))

(: index-within-range? (Integer Integer -> (Integer -> Boolean)))
(define ((index-within-range? low high) i)
  (and (>= i low) (< i high)))

(: angle-distance (Flonum Flonum -> Flonum))
(define (angle-distance a b)
  (abs
   (subtract tau
             (abs (- a b)))))
