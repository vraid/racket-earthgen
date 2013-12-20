#lang typed/racket

(provide (all-defined-out))

(require "dynamic-grid-structs.rkt")

(: edge-count ((U tile corner) -> Index))
(define (edge-count t)
  (if (tile? t)
      (vector-length (tile-tiles t))
      3))

(: tile-index (tile Integer -> Index))
(define (tile-index t n)
  (modulo n (edge-count t)))

(: tile-tile (tile Integer -> maybe-tile))
(define (tile-tile t n)
  (vector-ref (tile-tiles t)
              (tile-index t n)))

(: tile-corner (tile Integer -> maybe-corner))
(define (tile-corner t n)
  (vector-ref (tile-corners t)
              (tile-index t n)))

(: corner-index (corner Integer -> Index))
(define (corner-index t n)
  (modulo n (edge-count t)))

(: corner-tile (corner Integer -> maybe-tile))
(define (corner-tile t n)
  (vector-ref (corner-tiles t)
              (corner-index t n)))

(: corner-corner (corner Integer -> maybe-corner))
(define (corner-corner t n)
  (vector-ref (corner-corners t)
              (corner-index t n)))

(: member-index (All (a) (a (Vectorof (U False a)) -> Index)))
(define (member-index n v)
  (let ([a (vector-memq n v)])
    (if (not a)
        789135 ; almost always fails on 6-length vector ref
        a)))

(: tile-tile-index (tile tile -> Index))
(define (tile-tile-index t n)
  (member-index n (tile-tiles t)))

(: tile-corner-index (tile corner -> Index))
(define (tile-corner-index t n)
  (member-index n (tile-corners t)))

(: corner-tile-index (corner tile -> Index))
(define (corner-tile-index t n)
  (member-index n (corner-tiles t)))

(: corner-corner-index (corner corner -> Index))
(define (corner-corner-index t n)
  (member-index n (corner-corners t)))
