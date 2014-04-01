#lang typed/racket

(provide (all-defined-out))

(require "flvector3.rkt"
         "color.rkt")

(define-type tile-vector (Vectorof tile))
(define-type corner-vector (Vectorof corner))
(define-type tile-set (Setof tile))

(struct: color
  ([red : Byte]
   [green : Byte]
   [blue : Byte]
   [alpha : Byte]))

(struct: tile
  ([parent : (U tile corner)]
   [coordinates : flvector3]
   [tiles : tile-vector]
   [corners : corner-vector]
   [color : Bytes])
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
  (make-bytes 4 0))

(define empty-tile
  (tile empty-corner (flvector3-zero) (vector) (vector) no-color))

(define (empty-tile? t)
  (eq? t empty-tile))

(define (empty-corner? c)
  (eq? c empty-corner))

(: empty-parent? (tile -> Boolean))
(define (empty-parent? t)
  (eq? (tile-parent t) empty-tile))
