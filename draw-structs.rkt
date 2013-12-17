#lang typed/racket

(provide (struct-out draw-tile)
         draw-tile-vector)

(require "color.rkt"
         "vector3.rkt")

(define-type draw-tile-vector (Vectorof draw-tile))

(struct: draw-tile
  ([color : flcolor]
   [center : flvector3]
   [corners : (Vector flvector3
                      flvector3
                      flvector3
                      flvector3
                      flvector3
                      flvector3)])
  #:mutable)
