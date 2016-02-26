#lang typed/racket

(provide (all-defined-out))

(require "geometry-structs.rkt"
         "../grid.rkt"
         vraid/types
         vraid/math
         math/flonum)

(define default-axis (flvector 0.0 0.0 1.0))

(: coordinate-latitude (flvector3 flvector3 -> Flonum))
(define (coordinate-latitude axis v)
  (- (/ pi 2.0)
     (acos (flvector3-dot-product axis v))))

(: tile-latitude (planet-geometry integer -> Flonum))
(define (tile-latitude p n)
  (coordinate-latitude (planet-axis p) (tile-coordinates p n)))

(: northern-hemisphere? (flvector3 flvector3 -> Boolean))
(define (northern-hemisphere? axis v)
  (< 0.0 (coordinate-latitude axis v)))

(: southern-hemisphere? (flvector3 flvector3 -> Boolean))
(define (southern-hemisphere? axis v)
  (> 0.0 (coordinate-latitude axis v)))

(: edge-length (planet-geometry integer -> Flonum))
(define (edge-length p n)
  (* (planet-radius p)
     (flvector3-angle (corner-coordinates p (edge-corner p n 0))
                      (corner-coordinates p (edge-corner p n 1)))))

(: tile-edge-length (planet-geometry integer integer -> Flonum))
(define (tile-edge-length p n i)
  (* (planet-radius p)
     (edge-length p (tile-edge p n i))))

(: edge-segment (planet-geometry integer -> flvector3))
(define (edge-segment p n)
  (flvector3-subtract (corner-coordinates p (edge-corner p n 1))
                      (corner-coordinates p (edge-corner p n 0))))

(: tile-edge-segment (planet-geometry integer integer -> flvector3))
(define (tile-edge-segment p n i)
  (flvector3-subtract (corner-coordinates p (tile-corner p n i))
                      (corner-coordinates p (tile-corner p n (+ i 1)))))

(: tile-corner-vector (planet-geometry integer integer -> flvector3))
(define (tile-corner-vector p n i)
  (flvector3-subtract (tile-coordinates p n)
                      (corner-coordinates p (tile-corner p n i))))

(: edge-tile-distance (planet-geometry integer -> Flonum))
(define (edge-tile-distance p n)
  (* (planet-radius p)
     (flvector3-angle (tile-coordinates p (edge-tile p n 0))
                      (tile-coordinates p (edge-tile p n 1)))))

(: tile-tile-distance (planet-geometry integer integer -> Flonum))
(define (tile-tile-distance p n i)
  (* (planet-radius p)
     (edge-tile-distance p (tile-edge p n i))))

(: tile-corner-angle (planet-geometry integer integer -> Flonum))
(define (tile-corner-angle p n i)
  (* tau
     (exact->inexact
      (/ i (tile-edge-count n)))))

(: tile-edge-angle (planet-geometry integer integer -> Flonum))
(define (tile-edge-angle p n i)
  (let ([count (tile-edge-count n)])
    (* tau
       (exact->inexact
        (/ (+ i (/ 1 2 count))
           count)))))

(: spherical-angle (flvector3 flvector3 flvector3 -> Flonum))
(define (spherical-angle ref a b)
  (flvector3-angle (flvector3-rejection ref a)
                   (flvector3-rejection ref b)))

(: triangle-corner-angle (flvector3 flvector3 flvector3 -> Flonum))
(define (triangle-corner-angle ref a b)
  (spherical-angle ref
                   (flvector3-subtract ref a)
                   (flvector3-subtract ref b)))

(: spherical-triangle-excess (flvector3 flvector3 flvector3 -> Flonum))
(define (spherical-triangle-excess a b c)
  (subtract pi (+ (triangle-corner-angle a b c)
                  (triangle-corner-angle b c a)
                  (triangle-corner-angle c a b))))

(define spherical-triangle-area spherical-triangle-excess)

(: planet-radius-squared (planet-geometry -> Flonum))
(define (planet-radius-squared p)
  (let ([r (planet-radius p)])
    (* r r)))

(: planet-circumference (planet-geometry -> Flonum))
(define (planet-circumference p)
  (* tau (planet-radius p)))

(: n-gon-area (flonum grid integer -> Flonum))
(define (n-gon-area radius grid n)
  (: tile-segment-area (integer -> Flonum))
  (define (tile-segment-area i)
    (spherical-triangle-area (tile-coordinates grid n)
                             (corner-coordinates grid (tile-corner grid n i))
                             (corner-coordinates grid (tile-corner grid n (+ 1 i)))))
  (* (flexpt radius 2.0)
     (fl (apply + (map tile-segment-area (range (tile-edge-count n)))))))

(: tile-polar? (planet-geometry integer -> Boolean))
(define (tile-polar? p n)
  (let* ([v (tile-coordinates p n)]
         [axis (planet-axis p)]
         [corner-dist (flvector3-distance-squared v (corner-coordinates p (tile-corner p n 0)))])
    (or (> corner-dist (flvector3-distance-squared v axis))
        (> corner-dist (flvector3-distance-squared v (flvector3-negative axis))))))

(: tile-north (planet-geometry integer -> Flonum))
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

(: tile-south (planet-geometry integer -> Flonum))
(define (tile-south p n)
  (+ pi (tile-north p n)))

(: tile-west (planet-geometry integer -> Flonum))
(define (tile-west p n)
  (+ (* 0.5 pi) (tile-north p n)))

(: tile-east (planet-geometry integer -> Flonum))
(define (tile-east p n)
  (subtract (* 0.5 pi) (tile-north p n)))