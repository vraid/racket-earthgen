#lang racket

(require "utilities.rkt")
(require "grid-structs.rkt")

(provide tile-edge-count)
(provide tile-tile)
(provide tile-corner)
(provide tile-edge)
(provide tile-tile-position)
(provide tile-corner-position)
(provide tile-edge-position)

(provide corner-tile)
(provide corner-corner)
(provide corner-edge)
(provide corner-tile-position)
(provide corner-corner-position)
(provide corner-edge-position)

(provide edge-tile-sign)
(provide edge-corner-sign)

(provide subdivision-level-tile-count)
(provide subdivision-level-corner-count)
(provide subdivision-level-edge-count)
(provide grid-tile-count)
(provide grid-corner-count)
(provide grid-edge-count)

(provide grid-tile)
(provide grid-corner)
(provide grid-edge)

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

(define (tile-tile-position r t)
  (vector-index (tile-tiles->vector r) t))

(define (corner-tile-position c t)
  (vector-index (corner-tiles->vector c) t))

(define (tile-corner-position t c)
  (vector-index (tile-corners->vector t) c))

(define (corner-corner-position r c)
  (vector-index (corner-corners->vector r) c))

(define (tile-edge-position t e)
  (vector-index (tile-edges->vector t) e))

(define (corner-edge-position c e)
  (vector-index (corner-edges->vector c) e))

(define (edge-tile-sign e t)
  (case (vector-index (edge-tiles->vector e) t)
    [(0) 1]
    [(1) (- 1)]
    [else 0]))

(define (edge-corner-sign e c)
  (case (vector-index (edge-corners->vector e) c)
    [(0) 1]
    [(1) (- 1)]
    [else 0]))

(define (tile-tile t n)
  (vector-ref (tile-tiles->vector t) (modulo n (tile-edge-count t))))

(define (tile-corner t n)
  (vector-ref (tile-corners->vector t) (modulo n (tile-edge-count t))))

(define (tile-edge t n)
  (vector-ref (tile-edges->vector t) (modulo n (tile-edge-count t))))

(define (corner-tile c n)
  (vector-ref (corner-tiles->vector c) (modulo n 3)))

(define (corner-corner c n)
  (vector-ref (corner-corners->vector c) (modulo n 3)))

(define (corner-edge c n)
  (vector-ref (corner-edges->vector c) (modulo n 3)))
