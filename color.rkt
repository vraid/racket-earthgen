#lang typed/racket

(require math/flonum)

(provide color
         color?
         color-red
         color-green
         color-blue)

(struct: color
  ([red : Flonum]
   [green : Flonum]
   [blue : Flonum]))
