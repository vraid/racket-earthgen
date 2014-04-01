#lang racket

(provide coord->color)

(require "load-image.rkt"
         "projection.rkt"
         "flvector3.rkt"
         math/flonum)

; download http://snowy.arsc.alaska.edu/nasa/bmng/world_8km/world.topo.bathy.200405.3x5400x2700.jpg
; and replace the path
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
