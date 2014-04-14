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

(: angle-distance (Flonum Flonum -> Flonum))
(define (angle-distance a b)
  (abs
   (subtract tau
             (abs (- a b)))))
