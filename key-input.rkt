#lang racket

(provide key-input)

(require vraid/flow
         "map-modes.rkt"
         "planet/planet-generation.rkt")

(define (key-input control planet-handler update/repaint generate-terrain! color-planet! color-mode key-code)
  (define (zoom-out) (send control scale-by (/ 1.0 1.05)))
  (define (zoom-in) (send control scale-by 1.05))
  (match key-code
    ['escape (exit)]
    [#\q (generate-terrain!)]
    [#\w (and-let ([terrain (send planet-handler get-terrain)])
                  (thread
                   (thunk
                    (let* ([climate-func (delay (static-climate (default-climate-parameters) terrain))]
                           [initial (thunk ((force climate-func) #f))])
                      (send planet-handler
                            reset/climate
                            (thunk (force climate-func))
                            initial))
                    (update/repaint vegetation-map-mode))))]
    [#\e (thread
          (thunk
           (send planet-handler add/tick)
           (update/repaint color-mode)))]
    [#\r (when (send planet-handler ready?)
           (and-let ([planet (send planet-handler current)])
                    (thread
                     (thunk
                      (for ([n 15])
                        (displayln n)
                        (send planet-handler add/tick))
                      (update/repaint color-mode)))))]
    ['left (when (send planet-handler earlier)
             (update/repaint color-mode))]
    ['right (when (send planet-handler later)
              (update/repaint color-mode))]
    [#\a (color-planet! topography-map-mode)]
    [#\s (color-planet! vegetation-map-mode)]
    [#\d (color-planet! temperature-map-mode)]
    [#\f (color-planet! insolation-map-mode)]
    [#\g (color-planet! aridity-map-mode)]
    [#\h (color-planet! humidity-map-mode)]
    [#\j (color-planet! precipitation-map-mode)]
    [#\z (zoom-in)]
    [#\x (zoom-out)]
    ['wheel-up (zoom-in)]
    ['wheel-down (zoom-out)]
    [_ (void)]))
