#lang typed/racket

(require "types.rkt"
         "vector-util.rkt"
         "grid-structs.rkt"
         racket/fixnum)

(provide (all-defined-out))

(: subdivision-level-tile-count (index -> natural))
(define (subdivision-level-tile-count n)
  (+ 2 (* 10 (expt 3 n))))

(: subdivision-level-corner-count (index -> natural))
(define (subdivision-level-corner-count n)
  (* 20 (expt 3 n)))

(: subdivision-level-edge-count (index -> natural))
(define (subdivision-level-edge-count n)
  (* 30 (expt 3 n)))

(: grid-tile-count (grid -> natural))
(define (grid-tile-count grid)
  (subdivision-level-tile-count (grid-subdivision-level grid)))

(: grid-corner-count (grid -> natural))
(define (grid-corner-count grid)
  (subdivision-level-corner-count (grid-subdivision-level grid)))

(: grid-edge-count (grid -> natural))
(define (grid-edge-count grid)
  (subdivision-level-edge-count (grid-subdivision-level grid)))

(: grid-tile (grid index -> tile))
(define (grid-tile grid n)
  (vector-ref (grid-tiles->vector grid) n))

(: grid-corner (grid index -> corner))
(define (grid-corner grid n)
  (vector-ref (grid-corners->vector grid) n))

(: grid-edge (grid index -> edge))
(define (grid-edge grid n)
  (vector-ref (grid-edges->vector grid) n))

(: tile-edge-count (tile -> natural))
(define (tile-edge-count t)
  (if (> 12 (tile-id t)) 5 6))

(: corner-edge-count natural)
(define corner-edge-count 3)

(: tile-tile-position (tile index -> index))
(define (tile-tile-position r t)
  (vector-index t (tile-tiles->vector r)))

(: corner-tile-position (corner index -> index))
(define (corner-tile-position c t)
  (vector-index t (corner-tiles->vector c)))

(: tile-corner-position (tile index -> index))
(define (tile-corner-position t c)
  (vector-index c (tile-corners->vector t)))

(: corner-corner-position (corner index -> index))
(define (corner-corner-position r c)
  (vector-index c (corner-corners->vector r)))

(: tile-edge-position (tile index -> index))
(define (tile-edge-position t e)
  (vector-index e (tile-edges->vector t)))

(: corner-edge-position (corner index -> index))
(define (corner-edge-position c e)
  (vector-index e (corner-edges->vector c)))

(: edge-tile-sign (edge index -> Integer))
(define (edge-tile-sign e t)
  (case (vector-member t (edge-tiles->vector e))
    [(0) 1]
    [(1) -1]
    [else 0]))

(: edge-corner-sign (edge index -> Integer))
(define (edge-corner-sign e c)
  (case (vector-member c (edge-corners->vector e))
    [(0) 1]
    [(1) -1]
    [else 0]))

(: tile-tile (tile Integer -> index))
(define (tile-tile t n)
  (vector-ref (tile-tiles->vector t) (modulo n (tile-edge-count t))))

(: tile-corner (tile Integer -> index))
(define (tile-corner t n)
  (vector-ref (tile-corners->vector t) (modulo n (tile-edge-count t))))

(: tile-edge (tile Integer -> index))
(define (tile-edge t n)
  (vector-ref (tile-edges->vector t) (modulo n (tile-edge-count t))))

(: corner-tile (corner Integer -> index))
(define (corner-tile c n)
  (vector-ref (corner-tiles->vector c) (modulo n corner-edge-count)))

(: corner-corner (corner Integer -> index))
(define (corner-corner c n)
  (vector-ref (corner-corners->vector c) (modulo n corner-edge-count)))

(: corner-edge (corner Integer -> index))
(define (corner-edge c n)
  (vector-ref (corner-edges->vector c) (modulo n corner-edge-count)))

(: edge-tile (edge Integer -> index))
(define (edge-tile e n)
  (vector-ref (edge-tiles->vector e) (modulo n 2)))

(: edge-corner (edge Integer -> index))
(define (edge-corner e n)
  (vector-ref (edge-corners->vector e) (modulo n 2)))
