#lang typed/racket

(provide grid-closest-tile)

(require vraid/math
         "grid-structs.rkt"
         "grid-functions.rkt")

(: grid-tile-distance (grid FlVector -> (Integer -> Flonum)))
(define ((grid-tile-distance grid v) n)
  (flvector3-distance-squared v (tile-coordinates grid n)))

(: grid-closest-tile-of (grid FlVector (Listof Integer) -> Integer))
(define (grid-closest-tile-of grid v ls)
  (argmin (grid-tile-distance grid v) ls))

(: grid-step-to-closest (grid FlVector Integer -> Integer))
(define (grid-step-to-closest grid v start)
  (let ([closest (argmin (grid-tile-distance grid v) (cons start (grid-tile-tile-list grid start)))])
    (if (eq? closest start)
        start
        (grid-step-to-closest grid v closest))))

(: grid-closest-tile (grid (Option FlVector) -> (Option Integer)))
(define (grid-closest-tile grid v)
  (and v (let ([closest-pentagon (grid-closest-tile-of grid v (range 12))])
           (grid-step-to-closest grid v closest-pentagon))))
