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

(define (pixel-color bmap)
  (let* ([width (image-width bmap)]
         [height (image-height bmap)]
         [pixels (make-bytes (* 4 width height))])
    (begin
      (send bmap get-argb-pixels 0 0 (- width 1) (- height 1) pixels)
      (lambda (x y)
        (define (byte->fl b)
          (fl (/ b 255.0)))
        (let ([color (pixel pixels x y width)])
          (flcolor
           (byte->fl (send color red))
           (byte->fl (send color green))
           (byte->fl (send color blue))))))))

(define (pixel px x y width)
  (let ([offset (* 4 (+ x (* y (- width 1))))])
    (make-object color%
      (bytes-ref px (+ 1 offset))
      (bytes-ref px (+ 2 offset))
      (bytes-ref px (+ 3 offset)))))
