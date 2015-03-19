#lang typed/racket

(provide (all-defined-out))

(require vraid/struct
         "../grid/grid-structs.rkt")

(struct/kw: planet-geometry grid
            ([axis : FlVector]))

(define planet-axis planet-geometry-axis)

(: planet-radius (planet-geometry -> Flonum))
(define (planet-radius p)
  6371000.0)
