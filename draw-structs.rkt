#lang typed/racket

(require "color.rkt"
         "vector3.rkt")

(provide draw-tile
         draw-tile-color
         draw-tile-center
         draw-tile-corners
         draw-tile-vector
         set-draw-tile-color!)

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
