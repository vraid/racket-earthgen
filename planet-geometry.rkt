#lang typed/racket

(provide (all-defined-out))

(require "types.rkt"
         "math.rkt"
         "planet-structs.rkt"
         "planet-variables.rkt"
         "grid.rkt"
         "vector3.rkt")

(: coordinate-latitude (planet flvector3 -> Flonum))
(define (coordinate-latitude p v)
  (- (/ pi 2.0)
     (acos (flvector3-dot-product v (planet-axis p)))))

(: tile-latitude (planet index -> Flonum))
(define (tile-latitude p n)
  (coordinate-latitude p (tile-coordinates (grid-tile (planet-grid p) n))))

(: tile-corner-angle (index index -> Flonum))
(define (tile-corner-angle n i)
  (* tau
     (exact->inexact
      (/ i (tile-id-edge-count n)))))

(: tile-tile-angle (index index -> Flonum))
(define (tile-tile-angle n i)
  (let ([count (tile-id-edge-count n)])
    (* tau
       (exact->inexact
        (/ (+ i (/ 1 2 count))
           count)))))

(: tile-north (planet index -> Flonum))
(define (tile-north p n)
  (let* ([axis (planet-axis p)]
         [grid (planet-grid p)]
         [tile (grid-tile grid n)]
         [v (tile-coordinates tile)]
         [u (flvector3-rejection axis v)]
         [c (flvector3-rejection u (corner-coordinates (grid-corner grid (tile-corner tile 0))))]
         [angle (flvector3-angle axis c)]
         [cross (flvector3-cross-product u axis)]
         [sign (if (< (flvector3-distance-squared c cross)
                      (flvector3-distance-squared c (flvector3-negative cross)))
                   1.0
                   -1.0)])
    (* angle sign)))

(: tile-south (planet index -> Flonum))
(define (tile-south p n)
  (+ pi (tile-north p n)))

(: tile-west (planet index -> Flonum))
(define (tile-west p n)
  (+ (* 0.5 pi) (tile-north p n)))

(: tile-east (planet index -> Flonum))
(define (tile-east p n)
  (subtract (* 0.5 pi) (tile-north p n)))
