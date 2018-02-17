#lang racket

(require racket/class
         vraid/math
         math/flonum
         "control.rkt"
         "../planet/math/projection.rkt"
         "../planet/geometry-base.rkt")

(provide fixed-axis-control%)

(define fixed-axis-control%
  (class planet-control%
    (super-new)
    (inherit scale-by)
    (inherit-field wheel-delta
                   mouse-down-position
                   set-ortho-projection
                   viewport-width
                   viewport-height
                   scale
                   on-update)
    (field [mouse-down-latitude 0.0]
           [mouse-down-longitude 0.0])
    (init-field [latitude 0.0]
                [longitude 0.0])
    (define/override (rotation-list planet)
      (let* ([a (quaternion->axis-angle (rotation planet))])
        (cons (radians->degrees (axis-angle-angle a))
              (flvector->list (axis-angle-axis a)))))
    (define rotation
      (lambda (planet)
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
        (set-ortho-projection (- mx) mx (- my) my -2.0 2.0)))
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
                                       (orthographic->spherical (flvector mx my))))))
    (define/override (mouse-down position)
      (set! mouse-down-position position)
      (set! mouse-down-latitude latitude)
      (set! mouse-down-longitude longitude))
    (define/override (mouse-drag from to)
      (let ([delta (vector-map - mouse-down-position to)])
        (set! longitude
              (fl+ mouse-down-longitude
                   (fl* (fl (vector-ref delta 0))
                        (fl/ pi (* scale -900.0)))))
        (set! latitude
              (max (fl/ pi -2.0)
                   (min (fl/ pi 2.0)
                        (fl+ mouse-down-latitude
                             (fl* (fl (vector-ref delta 1))
                                  (fl/ pi (* scale -740.0)))))))))))
