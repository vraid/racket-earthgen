#lang typed/racket

(require math/flonum)

(provide (struct-out heightmap))

(struct: heightmap
  ([tiles : FlVector]
   [corners : FlVector])
  #:transparent)
