#lang racket

(require vraid/flow
         vraid/opengl
         racket/gui/base
         "map-mode.rkt"
         "map-modes.rkt"
         "gui/map-mode-panel.rkt"
         "gui/tile-data-panel.rkt"
         "projections.rkt"
         "key-input-handler.rkt"
         "mouse-input-handler.rkt"
         "planet-canvas.rkt"
         "interface/fixed-axis-control.rkt"
         "interface/planet-renderer.rkt"
         "planet/planet.rkt")

(provide planet-display)

(define (planet-display planet)
  (let* ([width 800]
         [height 800]
         [frame
          (new frame%
               [label "earthgen"]
               [width width]
               [height height])]
         [frame-panel
          (new horizontal-panel%
               [parent frame])]
         [left-panel
          (new vertical-panel%
               [parent frame-panel]
               [min-width 300]
               [stretchable-width #f])]
         [tile-panel
          (new tile-data-panel%
               [parent left-panel]
               [stretchable-height #f]
               [planet planet])]
         [filler-panel
          (new panel%
               [parent left-panel]
               [min-height 0]
               [stretchable-height #t])])
    (letrec ([repaint
              (thunk (send canvas force-repaint))]
             [canvas-gl-context
              (λ (f)
                (send canvas with-gl-context f))]
             [set-color
              (λ (map-mode)
                (canvas-gl-context (thunk (send renderer set-colors (map-mode-function map-mode)))))]
             [map-mode-panel
              (new map-mode-panel%
                   [parent left-panel]
                   [min-height 60]
                   [stretchable-height #f]
                   [on-color-select (λ (a)
                                      (set-color a)
                                      (repaint))]
                   [color-modes (append
                                 terrain-map-modes
                                 climate-map-modes)])]
             [control
              (new fixed-axis-control%
                   [viewport-width width]
                   [viewport-height height]
                   [scale 1.0]
                   [scale-min 0.1]
                   [scale-max 100.0]
                   [on-update repaint]
                   [set-ortho-projection set-gl-ortho-projection])]
             [mouse-handler
              (new mouse-input-handler%
                   [on-down (λ (position)
                              (send control mouse-down position))]
                   [on-click (λ (position)
                               (and-let* ([coordinates (send control get-coordinates planet (vector-ref position 0) (vector-ref position 1))]
                                          [tile (grid-closest-tile planet coordinates)])
                                         (begin
                                           (send tile-panel update/tile tile)
                                           (repaint))))]
                   [on-drag (λ (from to)
                              (send control mouse-drag from to)
                              (repaint))])]
             [key-handler
              (let ([scale 1.05])
                (new key-input-handler%
                     [zoom-in (thunk (send control scale-by scale))] 
                     [zoom-out (thunk (send control scale-by (/ scale)))]
                     [exit (thunk (send frame show #f))]))]
             [canvas
              (new planet-canvas%
                   [parent frame-panel]
                   [min-width 400]
                   [min-height 400]
                   [milliseconds-between-frames 70.0]
                   [on-key-event (λ (key)
                                   (send key-handler on-key key))]
                   [on-mouse-event (λ (event)
                                     (send mouse-handler mouse-event event))]
                   [resize-viewport (λ (width height)
                                      (send control resize-viewport width height)
                                      (repaint))]
                   [paint (thunk
                           (send control set-projection)
                           (gl-rotate (send control rotation-list planet))
                           (send renderer render))])]
             [renderer
              (canvas-gl-context
               (thunk ((planet-renderer (λ (a) identity)) planet)))])
      (set-color landscape-map-mode)
      (send frame show #t)
      (send canvas focus)
      canvas)))
