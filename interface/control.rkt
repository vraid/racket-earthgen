#lang racket

(require racket/class
         vraid/math
         math/flonum)

(provide (all-defined-out))

(define planet-control%
  (class object%
    (super-new)
    (field [wheel-delta 0.0]
           [mouse-down-position (vector 0 0)])
    (init-field on-update
                set-ortho-projection
                viewport-width
                viewport-height
                scale
                [scale-max #f]
                [scale-min #f])
    (define/public (mouse-down position)
      (set! mouse-down-position position))
    (define/public (mouse-drag from to)
      #f)
    (define/public (resize-viewport width height)
      (set! scale (* scale (fl (/ viewport-width width))))
      (set! viewport-width width)
      (set! viewport-height height))
    (define/public (scale-by n)
      (set! scale (fl ((within-interval scale-min scale-max) (* scale n))))
      (on-update))
    (define/public (rotation-list planet)
      '(0.0 0.0 0.0 0.0))
    (define/public (set-projection)
      #f)))
