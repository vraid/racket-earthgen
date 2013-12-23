#lang racket

(require math/flonum
         racket/draw
         "color.rkt")

(provide load-image/file
         image-width
         image-height
         pixel-color)

(define (load-image/file path)
  (read-bitmap path))

(define (image-width bmap)
  (send bmap get-width))

(define (image-height bmap)
  (send bmap get-height))

(define (pixel px x y width)
  (let ([offset (* 4 (+ x (* y (- width 1))))])
    (subbytes px offset (+ 4 offset))))

(define (pixel-color bmap)
  (let* ([width (image-width bmap)]
         [height (image-height bmap)]
         [pixels (make-bytes (* 4 width height))])
    (begin
      (send bmap get-argb-pixels 0 0 (- width 1) (- height 1) pixels)
      (lambda (x y)
        (pixel pixels x y width)))))
