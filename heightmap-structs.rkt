#lang typed/racket

(require math/flonum)

(provide heightmap
         heightmap-tiles
         heightmap-corners)

(struct: heightmap
  ([tiles : FlVector]
   [corners : FlVector])
  #:transparent)
