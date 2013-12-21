#lang typed/racket

(provide (all-defined-out))

(require "vector3.rkt")

(define-type maybe-tile (U False tile))
(define-type maybe-corner (U False corner))
(define-type tile-vector (Vectorof maybe-tile))
(define-type corner-vector (Vectorof maybe-corner))
(define-type tile-set (Setof tile))

(struct: tile
  ([parent : (U False tile corner)]
   [coordinates : flvector3]
   [tiles : tile-vector]
   [corners : corner-vector])
  #:mutable)

(struct: corner
  ([coordinates : flvector3]
   [tiles : tile-vector]
   [corners : corner-vector])
  #:mutable)
