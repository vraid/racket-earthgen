#lang racket

(require vraid/typed-gl
         vraid/color
         "river-vertices.rkt"
         "../planet/grid-base.rkt")

(provide river-renderer)

(define river-color
  (flcolor3 0.11372549019607843 0.3058823529411765 0.5686274509803921))

(define ((river-renderer transform) planet)
  (let ([buffer
         (let* ([tile-count (tile-count planet)]
                [buffer (make-gl-buffer (* 24 tile-count) (* 36 tile-count))])
           (begin
             (init-buffer planet transform river-color buffer)
             (update-buffer planet transform buffer)
             buffer))])
    (new (class object%
           (super-new)
           (define/public (update)
             ((gl-buffer-bind buffer)))
           (define/public (render)
             (gl-cull-face 'back)
             ((gl-buffer-draw buffer)))))))
