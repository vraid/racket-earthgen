#lang racket

(require racket/gui/base
         vraid/math
         vraid/flow
         vraid/opengl
         "planet/planet.rkt"
         "planet/planet-generation.rkt"
         "planet-color.rkt"
         "sample-terrain.rkt"
         "gui/edit-panel.rkt"
         "interface/fixed-axis-control.rkt"
         "interface/grid-handler.rkt"
         "interface/planet-handler.rkt"
         "interface/planet-renderer.rkt"
         math/flonum)

(require plot
         profile
         profile/render-text)

(define mouse-moving? #f)
(define mouse-down? #f)
(define mouse-down-x 0)
(define mouse-down-y 0)
(define milliseconds-between-frames 70.0)
(define last-draw (current-inexact-milliseconds))

(define-values
  (display-width display-height)
  (get-display-size))

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

(define (set-status-message! str)
  (send status-message set-label str))

(define planet-handler (new planet-handler%
                            [max-elements 24]
                            [set-status set-status-message!]))
(define grid-handler (new-grid-handler))
(define default-grid-size 5)
(define color-mode color-topography)

(define (set-color-mode color-function)
  (set! color-mode
        (if (planet-color-valid? color-function (send planet-handler current))
            color-function
            color-topography)))

(define (color-planet! color-function)
  (set-color-mode color-function)
  (send canvas with-gl-context
        (thunk (send planet-renderer set-tile-colors color-mode)))
  (repaint!))

(define (generate-terrain size method axis)
  (let ([grids (send grid-handler get-grids size)])
    (send planet-handler
          terrain/scratch
          (thunk ((heightmap->planet (first grids)) (method grids) axis)))))

(define (update/repaint mode)
  (set-color-mode mode)
  (send canvas with-gl-context (thunk (send planet-renderer update/planet color-mode)))
  (repaint!))

(define (generate-terrain/repaint size method axis)
  (generate-terrain size method axis)
  (update/repaint color-topography))

(generate-terrain default-grid-size sample-terrain default-axis)

(define global-panel
  (let* ([panel (new vertical-panel%
                     [parent no-frame]
                     [stretchable-height #f])]
         [p (edit-panel panel 30 100)]
         [size-edit (p "grid size")])
    (send size-edit
          link
          (thunk (number->string (grid-subdivision-level (send planet-handler current))))
          (lambda (size)
            (let ([size (string->number size)])
              (when (and (integer? size)
                         (<= 0 size))
                (thread
                 (thunk
                  (generate-terrain/repaint size (load "terrain-gen.rkt") default-axis)))))))
    panel))

(define tile-panel
  (new vertical-panel%
       [parent no-frame]
       [stretchable-height #f]))

(define update-tile-panel
  (let* ([height 30]
         [label-width 100]
         [general-panel (new vertical-panel%
                             [parent tile-panel]
                             [stretchable-height #f])]
         [general (edit-panel general-panel height label-width)]
         [terrain-panel (new vertical-panel%
                             [parent tile-panel]
                             [stretchable-height #f])]
         [terrain (edit-panel terrain-panel height label-width)]
         [climate-panel (new vertical-panel%
                             [parent tile-panel]
                             [stretchable-height #f])]
         [climate (edit-panel climate-panel height label-width)]
         [id-edit (general "tile id")]
         [elevation-edit (terrain "elevation")]
         [temperature-edit (climate "temperature")]
         [absolute-humidity-edit (climate "absolute humidity")]
         [relative-humidity-edit (climate "relative humidity")]
         [precipitation-edit (climate "precipitation")]
         [aridity-edit (climate "aridity")])
    (lambda (tile)
      (let* ([planet (send planet-handler current)]
             [terrain (planet-terrain? planet)]
             [climate (planet-climate? planet)])
        (send general-panel show tile)
        (send terrain-panel show (and tile terrain))
        (send climate-panel show (and tile climate))
        (when tile
          (let ([link (lambda (panel get set) (send panel link get set))])
            (link id-edit (thunk (number->string tile)) (thunk* #f))
            (when terrain
              (link elevation-edit
                    (thunk (number->string (tile-elevation planet tile)))
                    (lambda (n)
                      (let ([num (exact->inexact (string->number n))])
                        (when (real? num)
                          (begin
                            ((tile-terrain-data-elevation-set! (planet-terrain-tile planet)) tile num)
                            (color-planet! color-mode)))))))
            (when climate
              (link temperature-edit
                    (thunk (number->string (tile-temperature planet tile)))
                    (thunk* #f))
              (link absolute-humidity-edit
                    (thunk (number->string (tile-humidity planet tile)))
                    (thunk* #f))
              (link relative-humidity-edit
                    (thunk (number->string (relative-humidity (tile-temperature planet tile)
                                                              (tile-humidity planet tile))))
                    (thunk* #f))
              (link precipitation-edit
                    (thunk (number->string (tile-precipitation planet tile)))
                    (thunk* #f))
              (link aridity-edit
                    (thunk (number->string (aridity (tile-temperature planet tile)
                                                    (tile-humidity planet tile))))
                    (thunk* #f)))))))))

(struct tab-choice
  (label panel))

(define info-panel-tabs
  (vector (tab-choice "global" global-panel)
          (tab-choice "tile" tile-panel)))

(define (init-info-panel)
  (send info-panel set (map tab-choice-label (vector->list info-panel-tabs)))
  (send info-panel set-tab 0))

(define (repaint!)
  (set! last-draw 0.0)
  (send canvas on-paint))

(define canvas
  (new
   (class* canvas% ()
     (inherit with-gl-context swap-gl-buffers)
     (define control (new fixed-axis-control%
                          [viewport-width 800]
                          [viewport-height 800]
                          [scale 1.0]
                          [scale-min 0.1]
                          [scale-max 100.0]))
     (define/override (on-paint)
       (when (< milliseconds-between-frames
                (- (current-inexact-milliseconds) last-draw))
         (thread
          (thunk
           (with-gl-context
            (thunk
             (send control set-projection)
             (gl-rotate (send control rotation-list (send planet-handler current)))
             (send planet-renderer render)
             (swap-gl-buffers)))
           (set! last-draw (current-inexact-milliseconds))))))
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
         (generate-terrain/repaint (grid-subdivision-level (send planet-handler current)) (load "terrain-gen.rkt") default-axis))))
     (define/override (on-char event)
       (define key-code (send event get-key-code))
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
                  (update/repaint color-supported-vegetation))))]
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
         [#\a (color-planet! color-topography)]
         [#\s (color-planet! color-supported-vegetation)]
         [#\d (color-planet! color-temperature)]
         [#\f (color-planet! color-insolation)]
         [#\g (color-planet! color-aridity)]
         [#\h (color-planet! color-humidity)]
         [#\j (color-planet! color-precipitation)]
         ['wheel-up (begin
                      (send control wheel-up)
                      (repaint!))]
         ['wheel-down (begin
                        (send control wheel-down)
                        (repaint!))]
         [_ (void)]))
     (define (tile-at x y)
       (and-let* ([planet (send planet-handler current)]
                  [v (send control get-coordinates planet x y)]
                  [distance (build-flvector (tile-count planet)
                                            (lambda (n) (flvector3-distance-squared v (tile-coordinates planet n))))])
         (foldl (lambda (n closest)
                  (if (< (flvector-ref distance n)
                         (flvector-ref distance closest))
                      n
                      closest))
                0
                (range (tile-count planet)))))
     
     (define/override (on-event event)
       (if (send event button-up? 'left)
           (begin
             (unless mouse-moving?
               (and-let* ([planet (send planet-handler current)]
                          [tile (tile-at (send event get-x)
                                         (send event get-y))])
                 (begin
                   (update-tile-panel tile)))
               (repaint!))
             (set! mouse-down? false)
             (set! mouse-moving? #f))
           (if mouse-down?
               (begin
                 (let ([current-x (send event get-x)]
                       [current-y (send event get-y)])
                   (begin
                     (when (or (not (= mouse-down-x current-x))
                               (not (= mouse-down-y current-y)))
                       (set! mouse-moving? true))))
                 (send control on-event event)
                 (repaint!))
               (if (send event button-down? 'left)
                   (begin
                     (set! mouse-down-x (send event get-x))
                     (set! mouse-down-y (send event get-y))
                     (set! mouse-down? true)
                     (send control on-event event))
                   (void)))))
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
                    [planet (thunk (send planet-handler current))]))))

(update/repaint color-topography)
