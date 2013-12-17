#lang typed/racket

(provide relative->rectangular
         equirectangular-projection)

(require racket/flonum)

(: relative->rectangular (Integer Integer -> (FlVector -> (Vectorof Integer))))
(define (relative->rectangular width height)
  (lambda: ([coord : FlVector])
    (let ([lon (flvector-ref coord 0)]
          [lat (flvector-ref coord 1)])
      (vector (max 0 (modulo (round  (inexact->exact (* width (/ (+ 1.0 lon) 2.0))))
                             width))
              (max 0 (modulo (round (inexact->exact (* height (+ 0.5 lat))))
                             height))))))

(: equirectangular-projection (Flonum Flonum -> FlVector))
(define (equirectangular-projection longitude latitude)
  (flvector (/ longitude pi) (/ (- latitude) pi)))
