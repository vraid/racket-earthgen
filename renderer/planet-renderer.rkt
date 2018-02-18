#lang racket

(require vraid/opengl
         "tile-renderer.rkt"
         "river-renderer.rkt"
         "planet-data.rkt")

(provide planet-renderer)

(define ((planet-renderer transform) data climate?)
  (let* ([tile-renderer ((tile-renderer transform) data)]
         [river-renderer (if climate? ((river-renderer transform) data) #f)])
    (new (class object%
           (super-new)
           (define/public (set-colors color-function)
             (send tile-renderer set-colors color-function)
             (when climate?
               (send river-renderer update)))
           (define/public (render)
             (gl-clear (list 0.0 0.0 0.0 0.0))
             (send tile-renderer render)
             (when climate?
               (send river-renderer render)))))))
