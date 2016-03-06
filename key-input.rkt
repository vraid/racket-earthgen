#lang racket

(provide key-input)

(require "map-modes.rkt")

(define (key-input control set-color update/repaint generate-terrain generate-climate key-code)
  (define (zoom-out) (send control scale-by (/ 1.0 1.05)))
  (define (zoom-in) (send control scale-by 1.05))
  (match key-code
    ['escape (exit)]
    [#\q (generate-terrain)]
    [#\w (generate-climate)]
    [#\a (set-color topography-map-mode)]
    [#\s (set-color vegetation-map-mode)]
    [#\d (set-color temperature-map-mode)]
    [#\f (set-color insolation-map-mode)]
    [#\g (set-color aridity-map-mode)]
    [#\h (set-color humidity-map-mode)]
    [#\j (set-color precipitation-map-mode)]
    [#\z (zoom-in)]
    [#\x (zoom-out)]
    ['wheel-up (zoom-in)]
    ['wheel-down (zoom-out)]
    [_ (void)]))
