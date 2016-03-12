#lang typed/racket

(provide (all-defined-out))

(require typed/racket/class
         vraid/math
         math/flonum
         "../point.rkt"
         "../planet/geometry.rkt")

(define planet-control%
  (class object%
    (super-new)
    (field [wheel-delta : Real 0.0]
           [mouse-down-position : point origin])
    (init-field [on-update : (-> Void)]
                [viewport-width : Integer]
                [viewport-height : Integer]
                [scale : Float]
                [scale-max : (Option Float) #f]
                [scale-min : (Option Float) #f])
    (: mouse-down (point -> Void))
    (define/public (mouse-down position)
      (set! mouse-down-position position))
    (: mouse-drag (point point -> Any))
    (define/public (mouse-drag from to)
      #f)
    (: resize-viewport (Integer Integer -> Void))
    (define/public (resize-viewport width height)
      (set! scale (* scale (fl (/ viewport-width width))))
      (set! viewport-width width)
      (set! viewport-height height))
    (: scale-by (Float -> Void))
    (define/public (scale-by n)
      (set! scale (fl ((within-interval scale-min scale-max) (* scale n))))
      (on-update))
    (: rotation-list (planet-geometry -> (Listof Real)))
    (define/public (rotation-list planet)
      (list))
    (: set-projection (-> Any))
    (define/public (set-projection)
      #f)))
