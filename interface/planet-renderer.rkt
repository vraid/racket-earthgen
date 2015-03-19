#lang racket

(provide planet-renderer%)

(require "tile-renderer.rkt"
         "river-renderer.rkt"
         vraid/opengl
         vraid/color
         "../planet/planet.rkt"
         racket/flonum
         ffi/cvector
         ffi/unsafe)

(define planet-renderer%
  (class object%
    (super-new)
    (init-field planet)
    (define tile-renderer
      (new tile-renderer%
           [planet planet]))
    (define river-renderer
      (new river-renderer%
           [planet planet]))
    (define/public (update/planet)
      (send tile-renderer resize-buffer)
      (send tile-renderer remake-buffer)
      (when (planet-climate? (planet))
        (send river-renderer resize-buffer)
        (send river-renderer remake-buffer)))
    (define/public (set-tile-colors color-function)
      (send tile-renderer set-tile-colors color-function))
    (define/public (render)
      (gl-clear (list 0.0 0.0 0.0 0.0))
      (send tile-renderer render)
      (when (planet-climate? (planet))
        (send river-renderer render)))))
