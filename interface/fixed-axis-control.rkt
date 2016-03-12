#lang typed/racket

(provide fixed-axis-control%)

(require typed/racket/class
         vraid/math
         math/flonum
         "control.rkt"
         "../point.rkt"
         "../planet/geometry.rkt"
         "../planet/math/projection.rkt")

(require/typed vraid/opengl
               [set-gl-ortho-projection (Float Float Float Float Float Float -> Void)])

(define fixed-axis-control%
  (class planet-control%
    (super-new)
    (inherit scale-by)
    (inherit-field wheel-delta
                   mouse-down-position
                   viewport-width
                   viewport-height
                   scale
                   on-update)
    (field [mouse-down-latitude : Float 0.0]
           [mouse-down-longitude : Float 0.0])
    (init-field [latitude : Float 0.0]
                [longitude : Float 0.0])
    (define/override (rotation-list planet)
      (let ([a (quaternion->axis-angle (rotation planet))])
        (cons (radians->degrees (axis-angle-angle a))
              (flvector->list (axis-angle-axis a)))))
    (: rotation (planet-geometry -> quaternion))
    (define rotation
      (lambda ([planet : planet-geometry])
        (let ([q axis-angle->quaternion])
          (quaternion-product
           (let ([axis (planet-axis planet)])
             (q (if (flvector3-parallel? default-axis axis)
                    (flvector 1.0 0.0 0.0)
                    (flvector3-cross-product axis default-axis))
                (flvector3-angle default-axis axis)))
           (q (flvector 0.0 0.0 1.0) longitude)
           (q (flvector 0.0 1.0 0.0) latitude)
           (q (flvector 0.0 0.0 -1.0) (/ pi 2))
           (q (flvector -1.0 0.0 0.0) (/ pi 2))))))
    (define/override (set-projection)
      (let ([mx (fl* (fl/ 1.0 scale) (exact->inexact (/ viewport-width viewport-height)))]
            [my (fl/ 1.0 scale)])
        (set-gl-ortho-projection (- mx) mx (- my) my -2.0 2.0)))
    (: get-coordinates (planet-geometry Integer Integer -> (Option FlVector)))
    (define/public (get-coordinates planet x y)
      (let ([mx (* 2.0
                   (exact->inexact (/ viewport-width viewport-height))
                   (fl/ 1.0 scale)
                   (fl- (exact->inexact (/ x viewport-width)) 0.5))]
            [my (fl/ (* -2.0 (- (exact->inexact (/ y viewport-height)) 0.5))
                     scale)])
        (if (< 1.0 (fl+ (flexpt mx 2.0) (flexpt my 2.0)))
            #f
            (quaternion-vector-product (rotation planet)
                                       (orthographic->spherical mx my)))))
    (define/override (mouse-down position)
      (set! mouse-down-position position)
      (set! mouse-down-latitude latitude)
      (set! mouse-down-longitude longitude))
    (define/override (mouse-drag from to)
      (let ([delta (point-subtract to mouse-down-position)])
        (set! longitude
              (fl+ mouse-down-longitude
                   (fl* (fl (point-x delta))
                        (fl/ pi (* scale -900.0)))))
        (set! latitude
              (max (fl/ pi -2.0)
                   (min (fl/ pi 2.0)
                        (fl+ mouse-down-latitude
                             (fl* (fl (point-y delta))
                                  (fl/ pi (* scale -740.0)))))))))))
