#lang typed/racket

(require math/flonum)

(struct: color
  ([red : Flonum]
   [green : Flonum]
   [blue : Flonum]))
