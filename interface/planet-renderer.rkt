#lang racket

(require vraid/opengl
         "tile-renderer.rkt"
         "river-renderer.rkt"
         "../planet/climate-base.rkt")

(provide planet-renderer)

(define ((planet-renderer transform) planet)
  (let* ([tile-renderer ((tile-renderer transform) planet)]
         [river-renderer ((river-renderer transform) planet)])
    (new (class object%
           (super-new)
           (define/public (set-colors color-function)
             (send tile-renderer set-colors color-function)
             (when (planet-climate? planet)
               (send river-renderer update)))
           (define/public (render)
             (gl-clear (list 0.0 0.0 0.0 0.0))
             (send tile-renderer render)
             (when (planet-climate? planet)
               (send river-renderer render)))))))
