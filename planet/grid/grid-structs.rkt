#lang typed/racket

(provide (all-defined-out))

(require racket/fixnum
         racket/vector
         vraid/types
         vraid/math)

(define-type get-grid-flvector3 (integer -> flvector3))
(define-type get-grid-integer (Integer Integer -> Integer))

(struct: grid
  ([subdivision-level : natural]
   [tile-coordinates : get-grid-flvector3]
   [corner-coordinates : get-grid-flvector3]
   [tile-tile : get-grid-integer]
   [tile-corner : get-grid-integer]
   [tile-edge : get-grid-integer]
   [corner-tile : get-grid-integer]
   [corner-corner : get-grid-integer]
   [corner-edge : get-grid-integer]
   [edge-tile : get-grid-integer]
   [edge-corner : get-grid-integer])
  #:transparent)
