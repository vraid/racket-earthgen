#lang racket

(require racket/gui/base
         vraid/flow
         vraid/opengl
         vraid/build
         "key-input-handler.rkt"
         "mouse-input-handler.rkt"
         "planet-canvas.rkt"
         "point.rkt"
         "planet/planet.rkt"
         "planet/planet-generation.rkt"
         "terrain-gen.rkt"
         "terrain-dsl.rkt"
         "map-mode.rkt"
         "map-modes.rkt"
         "gui/map-mode-panel.rkt"
         "gui/generation-parameters-panel.rkt"
         "gui/tile-data-panel.rkt"
         "interface/fixed-axis-control.rkt"
         "interface/grid-handler.rkt"
         "interface/planet-handler.rkt"
         "interface/planet-renderer.rkt")

(require plot
         profile
         profile/render-text)

(define (set-status-message! str)
  (send status-message set-label str))

(define (on-planet-change planet)
  (send map-mode-panel enable-modes planet)
  (recolor/repaint)
  (send generation-panel update/planet planet)
  (send generation-panel enable-buttons #t)
  (send tile-panel update/planet planet))

(define (on-generation-start)
  (send generation-panel enable-buttons #f))

(define planet-handler (new planet-handler%
                            [set-status set-status-message!]
                            [on-start on-generation-start]
                            [on-change on-planet-change]))

(define (current-planet)
  (send planet-handler current))

(define grid-handler (new-grid-handler))
(define color-mode topography-map-mode)

(define (generate-terrain/current-parameters)
  (generate-terrain (send generation-panel terrain-parameters)))

(define (on-fail a)
  (displayln a))

(define last-seed "")

(define (set-terrain-algorithms algorithms)
  (send generation-panel set-algorithms algorithms))

(define (reload-terrain-algorithms)
  (with-handlers
      ([pair? (lambda (a) (displayln a))])
    (let* ([algorithms (load-algorithms "terrain-generation")])
      (send generation-panel set-algorithms algorithms))))

(define (generate-terrain parameters)
  (let* ([size (terrain-parameters-grid-size parameters)]
         [radius (terrain-parameters-radius parameters)]
         [seed (terrain-parameters-seed parameters)]
         [sea-level (terrain-parameters-sea-level parameters)]
         [axis (terrain-parameters-axis parameters)]
         [grids (send grid-handler get-grids size)]
         [algorithm (eval-terrain-function (terrain-parameters-algorithm parameters))])
    (set! last-seed seed)
    (send planet-handler
          generate
          "generating terrain"
          (thunk (planet/sea-level sea-level ((heightmap->planet (first grids)) ((algorithm seed) grids) radius axis)))
          (thunk* (set-color-mode topography-map-mode))
          on-fail)))

(define (generate-climate)
  (and-let* ([terrain (send planet-handler current)]
             [_ (planet-terrain? terrain)])
    (thread
     (thunk
      (let* ([climate-func (thunk (singular-climate (send generation-panel climate-parameters) terrain set-status-message!))])
        (send planet-handler
              generate
              "generating climate"
              climate-func
              (thunk* (set-color-mode landscape-map-mode))
              on-fail))))))

(define (set-color-mode mode)
  (set! color-mode mode))

(define (set-color-mode/repaint mode)
  (when ((map-mode-condition mode) (current-planet))
    (set-color-mode mode)
    (recolor/repaint)))

(define (recolor/repaint)
  (when ((map-mode-condition color-mode) (current-planet))
    (send map-mode-panel select-mode color-mode) 
    (send canvas with-gl-context
          (thunk (send planet-renderer update/planet (current-planet) (map-mode-function color-mode))))
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
       [on-select set-color-mode/repaint]
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

(define generation-panel
  (new generation-parameters-panel%
       [parent no-frame]
       [stretchable-height #f]
       [current-planet current-planet]
       [current-seed (thunk last-seed)]
       [generate-terrain (lambda (b e)
                           (generate-terrain/current-parameters))]
       [generate-climate (lambda (b e)
                           (generate-climate))]))

(define tile-panel
  (let ([p (new tile-data-panel%
                [parent no-frame]
                [stretchable-height #f]
                [current-planet current-planet])])
    (send p update/planet (current-planet))
    p))

(struct tab-choice
  (label panel))

(define info-panel-tabs
  (vector (tab-choice "global" generation-panel)
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
                              [coordinates (send control get-coordinates planet (point-x position) (point-y position))]
                              [tile (grid-closest-tile planet coordinates)])
                     (begin
                       (send generation-panel select-axis coordinates)
                       (send tile-panel update/tile tile)
                       (send canvas force-repaint))))]
       [on-drag (lambda (from to)
                  (send control mouse-drag from to)
                  (repaint!))]))

(define key-input-handler
  (let ([scale 1.05])
    (new key-input-handler%
         [set-color set-color-mode/repaint]
         [generate-terrain generate-terrain/current-parameters]
         [generate-climate generate-climate]
         [zoom-in (thunk (send control scale-by scale))] 
         [zoom-out (thunk (send control scale-by (/ scale)))])))

(define canvas
  (new planet-canvas%
       [parent frame-panel]
       [min-width 400]
       [min-height 400]
       [milliseconds-between-frames 70.0]
       [on-key-event (lambda (key)
                       (send key-input-handler on-key key))]
       [on-mouse-event (lambda (event)
                         (send mouse-input-handler mouse-event event))]
       [resize-viewport (lambda (width height)
                          (send control resize-viewport width height))]
       [paint (thunk
               (send control set-projection)
               (gl-rotate (send control rotation-list (current-planet)))
               (send planet-renderer render (current-planet)))]))

(init-info-panel)
(send frame maximize #t)
(send frame show #t)
(send canvas focus)

(define planet-renderer
  (send canvas with-gl-context
        (thunk (new planet-renderer%))))

(with-handlers
    ([pair? (lambda (a) (displayln a))])
  (let* ([algorithms (load-algorithms "terrain-generation")])
    (set-terrain-algorithms algorithms)
    (unless (hash-empty? algorithms)
      (generate-terrain (terrain-parameters/kw
                         #:algorithm (send generation-panel current-algorithm)
                         #:seed "skd"
                         #:grid-size 5
                         #:radius default-radius
                         #:sea-level 0.0
                         #:axis default-axis)))))
