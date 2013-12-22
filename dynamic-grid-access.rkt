#lang typed/racket

(provide (all-defined-out))

(require "dynamic-grid-structs.rkt"
         "vector3.rkt")

(: edge-count ((U tile corner) -> Index))
(define (edge-count t)
  (if (tile? t)
      (vector-length (tile-tiles t))
      3))

(: coordinates ((U tile corner) -> flvector3))
(define (coordinates t)
  (if (tile? t)
      (tile-coordinates t)
      (corner-coordinates t)))

(: parent-corner-at (corner Integer -> (U tile corner)))
(define (parent-corner-at c i)
  (let ([n (corner-index c (quotient (modulo i 6) 2))])
    (if (even? i)
        (corner-corner c n)
        (corner-tile c n))))

(: parent-at ((U tile corner) Integer -> (U tile corner)))
(define (parent-at t i)
  (if (tile? t)
      (tile-corner t i)
      (parent-corner-at t i)))

(: parent-parent-index ((U tile corner) (U tile corner) -> Index))
(define (parent-parent-index t n)
  (if (tile? t)
      (if (corner? n)
          (tile-corner-index t n)
          121235)
      (if (tile? n)
          (modulo (+ 1 (* 2 (corner-tile-index t n))) 6)
          (modulo (* 2 (corner-corner-index t n)) 6))))

(: tile-index (tile Integer -> Index))
(define (tile-index t n)
  (modulo n (edge-count t)))

(: tile-tile (tile Integer -> tile))
(define (tile-tile t n)
  (vector-ref (tile-tiles t)
              (tile-index t n)))

(: tile-corner (tile Integer -> corner))
(define (tile-corner t n)
  (vector-ref (tile-corners t)
              (tile-index t n)))

(: corner-index (corner Integer -> Index))
(define (corner-index t n)
  (modulo n (edge-count t)))

(: corner-tile (corner Integer -> tile))
(define (corner-tile t n)
  (vector-ref (corner-tiles t)
              (corner-index t n)))

(: corner-corner (corner Integer -> corner))
(define (corner-corner t n)
  (vector-ref (corner-corners t)
              (corner-index t n)))

(: member-index (All (a) (a (Vectorof a) -> Index)))
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
