#lang typed/racket

(require vraid/types
         "../planet-structs.rkt"
         racket/flonum)

(provide (all-defined-out))

(: tile-cloud-cover (planet integer -> Flonum))
(define (tile-cloud-cover p n)
  0.0)
