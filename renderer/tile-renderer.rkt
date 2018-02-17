#lang racket

(require vraid/typed-gl
         "tile-vertices.rkt"
         "../planet/grid-base.rkt")

(provide tile-renderer)

(define ((tile-renderer transform) planet)
  (let ([buffer
         (let* ([tile-count (tile-count planet)]
                [buffer (make-gl-buffer (* 7 tile-count) (* 18 tile-count))])
           (begin
             (init-buffer planet transform buffer)
             buffer))])
    (new (class object%
           (super-new)
           (define/public (set-colors color-function)
             (update-vertices! (tile-count planet)
                               buffer
                               (curry color-function planet))
             ((gl-buffer-bind buffer)))
           (define/public (render)
             (gl-cull-face 'back)
             ((gl-buffer-draw buffer)))))))
