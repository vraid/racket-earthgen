#lang racket

(require math/flonum)

(provide heightmap
         heightmap-tiles
         heightmap-corners)

(struct heightmap
  (tiles
   corners)
  #:transparent)
