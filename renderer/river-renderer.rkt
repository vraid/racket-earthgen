#lang racket

(require vraid/typed-gl
         vraid/color
         "planet-data.rkt"
         "river-vertices.rkt")

(provide river-renderer)

(define river-color
  (flcolor3 0.11372549019607843 0.3058823529411765 0.5686274509803921))

(define ((river-renderer transform) data)
  (let ([buffer
         (let* ([tile-count (planet-data-tile-count data)]
                [buffer (make-gl-buffer (* 24 tile-count) (* 36 tile-count))])
           (begin
             (init-buffer data transform river-color buffer)
             (update-buffer data transform buffer)
             buffer))])
    (new (class object%
           (super-new)
           (define/public (update)
             ((gl-buffer-bind buffer)))
           (define/public (render)
             (gl-cull-face 'back)
             ((gl-buffer-draw buffer)))))))
