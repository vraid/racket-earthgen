#lang typed/racket

(provide fixed-axis-control%)

(require typed/racket/class
         typed/racket/gui
         vraid/math
         vraid/types
         math/flonum
         "control.rkt"
         "../planet/planet.rkt"
         "../planet/math/projection.rkt")

(require/typed vraid/opengl
               [set-gl-ortho-projection (Flonum Flonum Flonum Flonum Flonum Flonum -> Void)])

(define fixed-axis-control%
  (class planet-control%
    (super-new)
    (inherit update-input
             scale-by)
    (inherit-field mouse-down?
                   mouse-moving?
                   wheel-delta
                   mouse-down-position
                   mouse-position
                   last-mouse-position
                   viewport-width
                   viewport-height
                   scale)
    (field [mouse-down-latitude : Flonum 0.0]
           [mouse-down-longitude : Flonum 0.0])
    (init-field [latitude : Flonum 0.0]
                [longitude : Flonum 0.0])
    (define/override (rotation-list planet)
      (let ([a (quaternion->axis-angle (rotation planet))])
        (cons (radians->degrees (axis-angle-angle a))
              (flvector->list (axis-angle-axis a)))))
    (: rotation (planet -> FlVector))
    (define rotation
      (lambda: ([planet : planet])
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
    (: get-coordinates (planet Integer Integer -> (maybe FlVector)))
    (define/public (get-coordinates planet x y)
      (let ([mx (fl* 2.0 (fl* (fl* (fl/ 1.0 scale) (fl- (exact->inexact (/ x viewport-width)) 0.5)) (exact->inexact (/ viewport-width viewport-height))))]
            [my (fl* -2.0 (fl/ (fl- (exact->inexact (/ y viewport-height)) 0.5) scale))])
        (if (< 1.0 (fl+ (flexpt mx 2.0) (flexpt my 2.0)))
            #f
            (quaternion-vector-product (rotation planet)
                                       (orthographic->spherical mx my)))))
    (: wheel-down (-> Void))
    (define/public (wheel-down)
      (scale-by (/ 1 1.05)))
    (: wheel-up (-> Void))
    (define/public (wheel-up)
      (scale-by 1.05))
    (define/override (on-event event)
      (update-input event)
      (when (send event button-down? 'left)
        (begin
          (set! mouse-down-latitude latitude)
          (set! mouse-down-longitude longitude)))
      (when mouse-moving?
        (let ([delta (point-subtract mouse-position mouse-down-position)])
          (set! longitude
                (fl+ mouse-down-longitude
                     (fl* (fl (point-x delta))
                          (fl/ pi (* scale -900.0)))))
          (set! latitude
                (max (fl/ pi -2.0)
                     (min (fl/ pi 2.0)
                          (fl+ mouse-down-latitude
                               (fl* (fl (point-y delta))
                                    (fl/ pi (* scale -740.0))))))))))))
