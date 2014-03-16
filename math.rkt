#lang typed/racket

(provide (all-defined-out)
         (all-from-out "constants.rkt"))

(require "constants.rkt")

(: divide (case-> (Number Zero -> Zero)
                  (Flonum Flonum -> Flonum)
                  (Integer Integer -> Exact-Rational)
                  (Number Number -> Number)))
(define (divide a b)
  (/ b a))

(: subtract (case-> (Integer Integer -> Integer)
                    (Flonum Flonum -> Flonum)
                    (Number Number -> Number)))
(define (subtract a b)
  (- b a))

(: angle-distance (Flonum Flonum -> Flonum))
(define (angle-distance a b)
  (abs
   (subtract tau
             (abs (- a b)))))
