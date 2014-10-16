#lang typed/racket

(provide (all-defined-out))

(require "flvector3.rkt"
         math/flonum)

(define-type quaternion FlVector)

(define el 
  flvector-ref)

(: remap-to-vector (quaternion (Vectorof Integer) -> flvector3))
(define (remap-to-vector q m)
  (let ([elm (lambda: ([q : quaternion]
                       [m : (Vectorof Integer)]
                       [i : Integer])
               (el q (vector-ref m i)))])
    (flvector (elm q m 0) 
              (elm q m 1) 
              (elm q m 2))))

(: col (quaternion (Vectorof Integer) quaternion (Vectorof Integer) -> flvector3))
(define (col q m r n)
  (flvector3-map-mult (remap-to-vector q m) 
                      (remap-to-vector r n)))
