#lang typed/racket

(provide (all-defined-out)
         (all-from-out "constants.rkt"))

(require "constants.rkt")

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

(: closest-within (case-> (Flonum Flonum -> (Flonum -> Flonum))
                          (Integer Integer -> (Integer -> Integer))
                          (Real Real -> (Real -> Real))))
(define ((closest-within low high) num)
  (max low (min high num)))

(: ratio-within (Flonum Flonum -> (Flonum -> Flonum)))
(define ((ratio-within low high) num)
  (define closest ((closest-within low high) num))
  (/ (- closest low)
     high))

(: angle-distance (Flonum Flonum -> Flonum))
(define (angle-distance a b)
  (abs
   (subtract tau
             (abs (- a b)))))
