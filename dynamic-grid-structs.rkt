#lang typed/racket

(provide (all-defined-out))

(require "vector3.rkt"
         "color.rkt")

(define-type tile-vector (Vectorof tile))
(define-type corner-vector (Vectorof corner))
(define-type tile-set (Setof tile))

(struct: tile
  ([parent : (U tile corner)]
   [coordinates : flvector3]
   [tiles : tile-vector]
   [corners : corner-vector]
   [color : flcolor])
  #:mutable)

(struct: corner
  ([coordinates : flvector3]
   [tiles : tile-vector]
   [corners : corner-vector])
  #:mutable)

(struct: data
  ([color : flcolor]))

(define empty-corner
  (corner (flvector3-zero) (vector) (vector)))

(define no-color
  (flcolor 0.0 0.0 0.0))

(define empty-tile
  (tile empty-corner (flvector3-zero) (vector) (vector) no-color))

(define (empty-tile? t)
  (eq? t empty-tile))

(define (empty-corner? c)
  (eq? c empty-corner))

(: empty-parent? (tile -> Boolean))
(define (empty-parent? t)
  (eq? (tile-parent t) empty-tile))
