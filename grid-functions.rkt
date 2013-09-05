#lang racket

(require "utilities.rkt"
         "grid-structs.rkt"
         racket/fixnum)

(provide tile-edge-count
         tile-tile
         tile-corner
         tile-edge
         tile-tile-position
         tile-corner-position
         tile-edge-position
         
         corner-edge-count
         corner-tile
         corner-corner
         corner-edge
         corner-tile-position
         corner-corner-position
         corner-edge-position
         
         edge-tile-sign
         edge-corner-sign
         
         subdivision-level-tile-count
         subdivision-level-corner-count
         subdivision-level-edge-count
         grid-tile-count
         grid-corner-count
         grid-edge-count
         
         grid-tile
         grid-corner
         grid-edge)

(define (subdivision-level-tile-count n)
  (+ 2 (* 10 (expt 3 n))))

(define (subdivision-level-corner-count n)
  (* 20 (expt 3 n)))

(define (subdivision-level-edge-count n)
  (* 30 (expt 3 n)))

(define (grid-tile-count grid)
  (subdivision-level-tile-count (grid-subdivision-level grid)))

(define (grid-corner-count grid)
  (subdivision-level-corner-count (grid-subdivision-level grid)))

(define (grid-edge-count grid)
  (subdivision-level-edge-count (grid-subdivision-level grid)))

(define (grid-tile grid n)
  (vector-ref (grid-tiles->vector grid) n))

(define (grid-corner grid n)
  (vector-ref (grid-corners->vector grid) n))

(define (grid-edge grid n)
  (vector-ref (grid-edges->vector grid) n))

(define (tile-edge-count t)
  (if (> 12 (tile-id t)) 5 6))

(define corner-edge-count 3)

(define (tile-tile-position r t)
  (vector-member t (tile-tiles->vector r)))

(define (corner-tile-position c t)
  (vector-member t (corner-tiles->vector c)))

(define (tile-corner-position t c)
  (vector-member c (tile-corners->vector t)))

(define (corner-corner-position r c)
  (vector-member c (corner-corners->vector r)))

(define (tile-edge-position t e)
  (vector-member e (tile-edges->vector t)))

(define (corner-edge-position c e)
  (vector-member e (corner-edges->vector c)))

(define (edge-tile-sign e t)
  (case (vector-member t (edge-tiles->vector e))
    [(0) 1]
    [(1) (- 1)]
    [else 0]))

(define (edge-corner-sign e c)
  (case (vector-member c (edge-corners->vector e))
    [(0) 1]
    [(1) (- 1)]
    [else 0]))

(define (tile-tile t n)
  (vector-ref (tile-tiles->vector t) (fxmodulo n (tile-edge-count t))))

(define (tile-corner t n)
  (vector-ref (tile-corners->vector t) (fxmodulo n (tile-edge-count t))))

(define (tile-edge t n)
  (vector-ref (tile-edges->vector t) (fxmodulo n (tile-edge-count t))))

(define (corner-tile c n)
  (vector-ref (corner-tiles->vector c) (fxmodulo n corner-edge-count)))

(define (corner-corner c n)
  (vector-ref (corner-corners->vector c) (fxmodulo n corner-edge-count)))

(define (corner-edge c n)
  (vector-ref (corner-edges->vector c) (fxmodulo n corner-edge-count)))
