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
  (coordinate-latitude p (tile-coordinates p n)))

(: tile-corner-angle (planet index index -> Flonum))
(define (tile-corner-angle p n i)
  (* tau
     (exact->inexact
      (/ i (tile-edge-count n)))))

(: tile-tile-angle (planet index index -> Flonum))
(define (tile-tile-angle p n i)
  (let ([count (tile-edge-count n)])
    (* tau
       (exact->inexact
        (/ (+ i (/ 1 2 count))
           count)))))

(: tile-polar? (planet index -> Boolean))
(define (tile-polar? p n)
  (let* ([v (tile-coordinates p n)]
         [axis (planet-axis p)]
         [corner-dist (flvector3-distance-squared v (corner-coordinates p (tile-corner p n 0)))])
    (or (> corner-dist (flvector3-distance-squared v axis))
        (> corner-dist (flvector3-distance-squared v (flvector3-negative axis))))))

(: tile-north (planet index -> Flonum))
(define (tile-north p n)
  (let* ([axis (planet-axis p)]
         [v (tile-coordinates p n)]
         [u (flvector3-rejection axis v)]
         [c (flvector3-rejection u (corner-coordinates p (tile-corner p n 0)))]
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
