#lang racket

(provide coord->color)

(require "load-image.rkt"
         "projection.rkt"
         "vector3.rkt"
         math/flonum)

(define image (load-image/file "C:/Users/vraid/Pictures/earth-satellite.jpg"))
(define rel->rect
  (relative->rectangular (image-width image) (image-height image)))

(define (latitude v)
  (- (/ pi 2.0)
     (acos (flvector3-dot-product v (flvector 0.0 0.0 1.0)))))

(define (longitude v)
  (let* ([x (flvector-ref v 0)]
         [y (flvector-ref v 1)])
    (atan y x)))

(define pxcolor (pixel-color image))

(define (coord->color coordinates)
  (let* ([lat (latitude coordinates)]
         [lon (longitude coordinates)]
         [coord (equirectangular-projection lon lat)]
         [px (rel->rect coord)]
         [x (vector-ref px 0)]
         [y (vector-ref px 1)])
    (pxcolor x y)))
