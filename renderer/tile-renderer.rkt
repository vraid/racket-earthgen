#lang racket

(require vraid/typed-gl
         "planet-data.rkt"
         "tile-vertices.rkt")

(provide tile-renderer)

(define ((tile-renderer transform) data)
  (let* ([tile-count (planet-data-tile-count data)]
         [buffer
          (let* ([buffer (make-gl-buffer (* 7 tile-count) (* 18 tile-count))])
            (begin
              (init-buffer
               data      
               transform
               buffer)
              buffer))])
    (new (class object%
           (super-new)
           (define/public (set-colors color-function)
             (update-vertices! tile-count
                               buffer
                               color-function)
             ((gl-buffer-bind buffer)))
           (define/public (render)
             (gl-cull-face 'back)
             ((gl-buffer-draw buffer)))))))
