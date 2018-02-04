#lang typed/racket

(provide (all-defined-out))

(struct: heightmap
  ([tiles : FlVector]
   [corners : FlVector]))
