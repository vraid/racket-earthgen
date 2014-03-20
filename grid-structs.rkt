#lang typed/racket

(provide (all-defined-out))

(require racket/fixnum
         racket/vector
         "types.rkt"
         "vector3.rkt")

(define-type get-grid-flvector3 (index -> flvector3))
(define-type get-grid-index (Integer Integer -> Integer))

(struct: grid
  ([subdivision-level : natural]
   [tile-coordinates : get-grid-flvector3]
   [corner-coordinates : get-grid-flvector3]
   [tile-tile : get-grid-index]
   [tile-corner : get-grid-index]
   [tile-edge : get-grid-index]
   [corner-tile : get-grid-index]
   [corner-corner : get-grid-index]
   [corner-edge : get-grid-index]
   [edge-tile : get-grid-index]
   [edge-corner : get-grid-index])
  #:transparent)
