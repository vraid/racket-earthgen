#lang racket

(provide (all-defined-out))

(require vraid/math
         vraid/opengl
         "../planet/planet.rkt"
         "../planet/math/projection.rkt"
         math/flonum)

(struct point
  (x y)
  #:transparent)

(define (point-equal? p q)
  (and (equal? (point-x p) (point-x q))
       (equal? (point-y p) (point-y q))))

(define (point-subtract p q)
  (point (- (point-x q) (point-x p))
         (- (point-y q) (point-y p))))

(define planet-control%
  (class* object% ()
    (super-new)
    (field [mouse-down? #f]
           [mouse-moving? #f]
           [wheel-delta #f]
           [mouse-down-position #f]
           [mouse-position #f]
           [last-mouse-position #f])
    (init-field viewport-width
                viewport-height
                scale
                [scale-max #f]
                [scale-min #f])
    (define/public (on-event event)
      #f)
    (define/public (resize-viewport width height)
      (set! scale (* scale (/ viewport-width width)))
      (set! viewport-width width)
      (set! viewport-height height))
    (define/public (scale-by n)
      (set! scale ((within-interval scale-min scale-max) (* scale n))))
    (define/public (rotation-list planet)
      (list))
    (define/public (set-projection)
      #f)
    (define (mouse-up)
      (set! mouse-down? #f)
      (set! mouse-moving? #f)
      (set! mouse-down-position #f)
      (set! mouse-position #f)
      (set! last-mouse-position #f))
    (define/public (update-input event)
      (when (send event button-up? 'left)
        (mouse-up))
      (set! last-mouse-position mouse-position)
      (set! mouse-position (point (send event get-x) (send event get-y)))
      (when mouse-down?
        (when (not (point-equal? last-mouse-position mouse-position))
          (set! mouse-moving? #t)))
      (when (send event button-down? 'left)
        (begin
          (set! mouse-down-position mouse-position)
          (set! mouse-down? #t))))))

(define fixed-axis-control%
  (class* planet-control% ()
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
    (init-field [latitude 0.0]
                [longitude 0.0])
    (field [mouse-down-latitude #f]
           [mouse-down-longitude #f])
    (define/override (rotation-list planet)
      (let ([a (quaternion->axis-angle (rotation planet))])
        (cons (radians->degrees (axis-angle-angle a))
              (flvector->list (axis-angle-axis a)))))
    (define (rotation planet)
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
         (q (flvector -1.0 0.0 0.0) (/ pi 2)))))
    (define/override (set-projection)
      (let ([mx (fl* (fl/ 1.0 scale) (exact->inexact (/ viewport-width viewport-height)))]
            [my (fl/ 1.0 scale)])
        (set-gl-ortho-projection (- mx) mx (- my) my -2.0 2.0)))
    (define/public (get-coordinates planet x y)
      (let ([mx (fl* 2.0 (fl* (fl* (fl/ 1.0 scale) (fl- (exact->inexact (/ x viewport-width)) 0.5)) (exact->inexact (/ viewport-width viewport-height))))]
            [my (fl* -2.0 (fl/ (fl- (exact->inexact (/ y viewport-height)) 0.5) scale))])
        (if (< 1.0 (fl+ (flexpt mx 2.0) (flexpt my 2.0)))
            #f
            (quaternion-vector-product (rotation planet)
                                       (orthographic->spherical mx my)))))
    (define/public (wheel-down)
      (scale-by (/ 1 1.05)))
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
                     (fl* (exact->inexact (point-x delta))
                          (fl/ pi (* scale -900.0)))))
          (set! latitude
                (max (fl/ pi -2.0)
                     (min (fl/ pi 2.0)
                          (fl+ mouse-down-latitude
                               (fl* (exact->inexact (point-y delta))
                                    (fl/ pi (* scale -740.0))))))))))))
