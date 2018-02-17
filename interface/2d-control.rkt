#lang racket

(require racket/class
         vraid/math
         math/flonum
         "control.rkt")

(provide 2d-control%)

(define 2d-control%
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
    (field [mouse-down-x 0.0]
           [mouse-down-y 0.0])
    (init-field spherical->projected
                projected->spherical
                [x 0.0]
                [y 0.0])
    (define (project vec)
      ((spherical->projected vec) vec))
    (define projection-width (abs (flvector-ref (project (flvector -1.0 0.0 0.0)) 0)))
    (define projection-height (flvector-ref (project (flvector 1.0 0.0 1.0)) 1))
    (define (limit-offset limit)
      (within-interval (- limit) limit))
    (define limit-x-offset (limit-offset projection-width))
    (define limit-y-offset (limit-offset projection-height))
    (define (pixel->relative a)
      (/ a (* 0.5 viewport-height)))
    (define (relative->pixel a)
      (* a (* 0.5 viewport-height)))
    (define/override (set-projection)
      (let* ([mx (fl* (fl/ 1.0 scale) (exact->inexact (/ viewport-width viewport-height)))]
             [my (fl/ 1.0 scale)]
             [x-offset (limit-x-offset (pixel->relative x))]
             [y-offset (limit-y-offset (- (pixel->relative y)))])
        (set-ortho-projection (- x-offset mx)
                              (+ x-offset mx)
                              (- y-offset my)
                              (+ y-offset my)
                              -2.0
                              2.0)))
    (define/public (get-coordinates planet x y)
      (flvector 0.0 0.0 0.0))
    (define/override (mouse-down position)
      (set! mouse-down-position position)
      (set! x (relative->pixel (limit-x-offset (pixel->relative x))))
      (set! y (relative->pixel (limit-y-offset (pixel->relative y))))
      (set! mouse-down-x x)
      (set! mouse-down-y y))
    (define/override (mouse-drag from to)
      (let ([delta (vector-map - mouse-down-position to)])
        (set! x (+ mouse-down-x (/ (vector-ref delta 0) scale)))
        (set! y (+ mouse-down-y (/ (vector-ref delta 1) scale)))))))
