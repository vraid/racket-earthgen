#lang racket

(require racket/gui/base
         vraid/flow
         vraid/opengl
         "key-input.rkt"
         "point.rkt"
         "mouse-input-handler.rkt"
         "load-terrain.rkt"
         "planet/planet.rkt"
         "planet/planet-generation.rkt"
         "gui/edit-panel.rkt"
         "map-mode.rkt"
         "map-modes.rkt"
         "gui/map-mode-panel.rkt"
         "tile-data-panel.rkt"
         "interface/fixed-axis-control.rkt"
         "interface/grid-handler.rkt"
         "interface/planet-handler.rkt"
         "interface/planet-renderer.rkt")

(require plot
         profile
         profile/render-text)

(define milliseconds-between-frames 70.0)
(define last-draw (current-inexact-milliseconds))

(define-values
  (display-width display-height)
  (get-display-size))

(define (set-status-message! str)
  (send status-message set-label str))

(define (on-planet-change planet)
  (send map-mode-panel enable-modes planet))

(define planet-handler (new planet-handler%
                            [max-elements 24]
                            [set-status set-status-message!]
                            [on-change on-planet-change]))

(define (current-planet)
  (send planet-handler current))

(define grid-handler (new-grid-handler))
(define color-mode topography-map-mode)

(define (generate-terrain size axis)
  (let ([grids (send grid-handler get-grids size)])
    (send planet-handler
          terrain/scratch
          (thunk (planet/sea-level 0.0 ((heightmap->planet (first grids)) ((load-terrain) grids) axis))))))

(define (update/repaint mode)
  (set-color-mode mode)
  (send canvas with-gl-context (thunk (send planet-renderer update/planet (map-mode-function color-mode))))
  (repaint!))

(define (generate-terrain/repaint size axis)
  (generate-terrain size axis)
  (update/repaint topography-map-mode))

(define (set-color-mode mode)
  (send map-mode-panel select-mode mode) 
  (set! color-mode mode))

(define (color-planet! mode)
  (when ((map-mode-condition mode) (current-planet))
    (set-color-mode mode)
    (send canvas with-gl-context
          (thunk (send planet-renderer set-tile-colors (map-mode-function color-mode))))
    (repaint!)))

(define-values
  (window-width window-height)
  (values 800 600))

(define no-frame
  (new frame%
       [label ""]))

(define frame
  (new frame%
       [label "earthgen"]
       [width window-width]
       [height window-height]))

(define frame-panel
  (new horizontal-panel%
       [parent frame]))

(define left-panel
  (new vertical-panel%
       [parent frame-panel]
       [min-width 300]
       [stretchable-width #f]))

(define info-panel
  (new (class* tab-panel% ()
         (super-instantiate ())
         (define/public (set-tab selection)
           (for ([n (vector-length info-panel-tabs)])
             (send (tab-choice-panel (vector-ref info-panel-tabs n))
                   reparent
                   (if (= n selection)
                       info-panel
                       no-frame)))))
       
       [choices (list)]
       [parent left-panel]
       [callback (lambda (panel event)
                   (send panel set-tab (send panel get-selection)))]))

(define map-mode-panel
  (new map-mode-panel%
       [parent left-panel]
       [min-height 60]
       [stretchable-height #f]
       [on-select color-planet!]
       [modes (list->vector
               (append
                terrain-map-modes
                climate-map-modes))]))

(define status-panel
  (new panel%
       [parent left-panel]
       [min-height 20]
       [stretchable-height #f]))

(define status-message
  (new message%
       [parent status-panel]
       [label "ready"]
       [auto-resize #t]))

(define global-panel
  (let* ([panel (new vertical-panel%
                     [parent no-frame]
                     [stretchable-height #f])]
         [p (edit-panel panel 30 100)]
         [size-edit (p "grid size")])
    (send size-edit
          link
          (thunk (number->string (grid-subdivision-level (current-planet))))
          (lambda (size)
            (let ([size (string->number size)])
              (when (and (integer? size)
                         (<= 0 size))
                (thread
                 (thunk
                  (generate-terrain/repaint size default-axis)))))))
    panel))

(define tile-panel
  (new vertical-panel%
       [parent no-frame]
       [stretchable-height #f]))

(define update-tile-panel (tile-data-panel tile-panel))

(struct tab-choice
  (label panel))

(define info-panel-tabs
  (vector (tab-choice "global" global-panel)
          (tab-choice "tile" tile-panel)))

(define (init-info-panel)
  (send info-panel set (map tab-choice-label (vector->list info-panel-tabs)))
  (send info-panel set-tab 0))

(define (repaint!)
  (send canvas force-repaint)
  (void))

(define control (new fixed-axis-control%
                     [viewport-width 800]
                     [viewport-height 800]
                     [scale 1.0]
                     [scale-min 0.1]
                     [scale-max 100.0]
                     [on-update repaint!]))

(define mouse-input-handler
  (new mouse-input-handler%
       [on-down (lambda (position)
                  (send control mouse-down position))]
       [on-click (lambda (position)
                   (and-let* ([planet (current-planet)]
                              [tile (grid-closest-tile planet (send control get-coordinates planet (point-x position) (point-y position)))])
                             (begin
                               (update-tile-panel planet tile)
                               (send canvas force-repaint))))]
       [on-drag (lambda (from to)
                  (send control mouse-drag from to)
                  (repaint!))]))

(define canvas
  (new
   (class* canvas% ()
     (inherit with-gl-context swap-gl-buffers)
     (define (paint)
       (thread
        (thunk
         (with-gl-context
          (thunk
           (send control set-projection)
           (gl-rotate (send control rotation-list (current-planet)))
           (send planet-renderer render)
           (swap-gl-buffers)))
         (set! last-draw (current-inexact-milliseconds)))))
     (define/override (on-paint)
       (when (< milliseconds-between-frames
                (- (current-inexact-milliseconds) last-draw))
         (paint)))
     (define/public (force-repaint)
       (paint))
     (define/override (on-size width height)
       (send control resize-viewport width height)
       (set! display-width width)
       (set! display-height height)
       (with-gl-context
        (thunk
         (set-gl-viewport 0 0 width height))))
     (define (generate-terrain!)
       (thread
        (thunk
         (generate-terrain/repaint (grid-subdivision-level (current-planet)) default-axis))))
     (define/override (on-char event)
       (define key-code (send event get-key-code))
       (key-input control planet-handler update/repaint generate-terrain! color-planet! color-mode key-code))
     (define/override (on-event event)
       (send mouse-input-handler mouse-event event))
     (super-instantiate () (style '(gl))))
   [parent frame-panel]
   [min-width 400]
   [min-height 400]))

(init-info-panel)
(send frame maximize #t)
(send frame show #t)
(send canvas focus)

(define planet-renderer
  (send canvas with-gl-context
        (thunk (new planet-renderer%
                    [planet current-planet]))))

(generate-terrain/repaint 5 default-axis)
