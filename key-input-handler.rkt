#lang racket

(provide key-input-handler%)

(require "map-modes.rkt")

(define key-input-handler%
  (class object%
    (super-new)
    (init-field zoom-in
                zoom-out
                set-color
                generate-terrain
                generate-climate)
    (define/public (on-key key-code)
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
        [_ (void)]))))
