#lang racket

(provide planet-renderer%)

(require vraid/opengl
         "tile-renderer.rkt"
         "river-renderer.rkt"
         "../planet/climate-base.rkt")

(define planet-renderer%
  (class object%
    (super-new)
    (define current-planet #f)
    (define tile-renderer (new tile-renderer%))
    (define river-renderer (new river-renderer%))
    (define/public (set-shapes planet transform)
      (send tile-renderer set-shapes planet transform)
      (when (planet-climate? planet)
        (send river-renderer set-shapes planet transform)))
    (define/public (set-colors planet transform color-function)
      (send tile-renderer set-colors planet color-function)
      (when (planet-climate? planet)
        (send river-renderer set-colors planet transform)))
    (define/public (update/planet planet transform color-function)
      (send tile-renderer resize-buffer planet)
      (send river-renderer resize-buffer planet)
      (set-shapes planet transform)
      (set-colors planet transform color-function))
    (define/public (render planet)
      (gl-clear (list 0.0 0.0 0.0 0.0))
      (send tile-renderer render)
      (when (planet-climate? planet)
        (send river-renderer render)))))
