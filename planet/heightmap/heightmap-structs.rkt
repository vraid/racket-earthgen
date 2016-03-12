#lang typed/racket

(provide (struct-out heightmap))

(require math/flonum)

(struct: heightmap
  ([tiles : FlVector]
   [corners : FlVector])
  #:transparent)
