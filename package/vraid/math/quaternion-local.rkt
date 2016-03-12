#lang typed/racket

(provide (all-defined-out))

(require "flvector3.rkt"
         math/flonum)

(define el 
  flvector-ref)

(: remap-to-vector (FlVector (Vectorof Integer) -> FlVector))
(define (remap-to-vector q m)
  (let ([elm (lambda: ([q : FlVector]
                       [m : (Vectorof Integer)]
                       [i : Integer])
               (el q (vector-ref m i)))])
    (flvector (elm q m 0) 
              (elm q m 1) 
              (elm q m 2))))

(: col (FlVector (Vectorof Integer) FlVector (Vectorof Integer) -> FlVector))
(define (col q m r n)
  (flvector3-map-mult (remap-to-vector q m) 
                      (remap-to-vector r n)))
