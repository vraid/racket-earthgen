#lang typed/racket

(provide (all-defined-out))

(require math/flonum)

(: col (FlVector (Vectorof Integer) FlVector (Vectorof Integer) -> FlVector))
(define (col v m u n)
  (flvector-map mult
                (remap v m)
                (remap u n)))

(: mult (Flonum Flonum * -> Flonum))
(define (mult a . n)
  (foldl * a n))

(: remap (FlVector (Vectorof Integer) -> FlVector))
(define (remap v m)
  (let ([elm (lambda: ([v : FlVector]
                       [m : (Vectorof Integer)]
                       [i : Integer])
               (flvector-ref v (vector-ref m i)))])
    (flvector (elm v m 0)
              (elm v m 1) 
              (elm v m 2))))
