#lang typed/racket

(provide (all-defined-out))

(require typed/racket/class
         typed/racket/gui
         vraid/math
         vraid/types
         vraid/flow
         math/flonum
         "../point.rkt"
         "../planet/planet.rkt")

(define planet-control%
  (class object%
    (super-new)
    (field [wheel-delta : Real 0.0]
           [mouse-down-position : point origin])
    (init-field [viewport-width : Integer]
                [viewport-height : Integer]
                [scale : Flonum]
                [scale-max : (maybe Flonum) #f]
                [scale-min : (maybe Flonum) #f])
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
    (: scale-by (Flonum -> Void))
    (define/public (scale-by n)
      (set! scale (fl ((within-interval scale-min scale-max) (* scale n)))))
    (: rotation-list (planet-geometry -> (Listof Real)))
    (define/public (rotation-list planet)
      (list))
    (: set-projection (-> Any))
    (define/public (set-projection)
      #f)))
