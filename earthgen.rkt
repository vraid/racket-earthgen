#lang racket

(require racket/gui/base
         vraid/math
         vraid/flow
         vraid/opengl
         "load-terrain.rkt"
         "planet/planet.rkt"
         "planet/planet-generation.rkt"
         "planet-color.rkt"
         "gui/edit-panel.rkt"
         "tile-data-panel.rkt"
         "interface/fixed-axis-control.rkt"
         "interface/grid-handler.rkt"
         "interface/planet-handler.rkt"
         "interface/planet-renderer.rkt"
         math/flonum)

(require plot
         profile
         profile/render-text)

(struct mouse-state
  (moving? down? down-x down-y)
  #:mutable)

(define current-mouse-state
  (mouse-state #f #f 0 0))

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

(define (generate-terrain size axis)
  (let ([grids (send grid-handler get-grids size)])
    (send planet-handler
          terrain/scratch
          (thunk (planet/sea-level 0.0 ((heightmap->planet (first grids)) ((load-terrain) grids) axis))))))

(define (update/repaint mode)
  (set-color-mode mode)
  (send canvas with-gl-context (thunk (send planet-renderer update/planet color-mode)))
  (repaint!))

(define (generate-terrain/repaint size axis)
  (generate-terrain size axis)
  (update/repaint color-topography))

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
  (set! last-draw 0.0)
  (send canvas on-paint)
  (void))

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
         (generate-terrain/repaint (grid-subdivision-level (send planet-handler current)) default-axis))))
     (define (zoom-in)
       (send control wheel-up)
       (repaint!))
     (define (zoom-out)
       (send control wheel-down)
       (repaint!))
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
         [#\l (color-planet! (color-area
                              (let ([planet (send planet-handler current)])
                                (stream-fold (lambda (a n)
                                               (max a (tile-area planet n)))
                                             0.0
                                             (in-range (tile-count planet))))))]
         [#\z (zoom-in)]
         [#\x (zoom-out)]
         ['wheel-up (zoom-in)]
         ['wheel-down (zoom-out)]
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
             (unless (mouse-state-moving? current-mouse-state)
               (and-let* ([planet (send planet-handler current)]
                          [tile (tile-at (send event get-x)
                                         (send event get-y))])
                         (begin
                           (update-tile-panel planet tile)))
               (repaint!))
             (set-mouse-state-down?! current-mouse-state #f)
             (set-mouse-state-moving?! current-mouse-state #f))
           (if (mouse-state-down? current-mouse-state)
               (begin
                 (let ([current-x (send event get-x)]
                       [current-y (send event get-y)])
                   (begin
                     (when (not (and (= (mouse-state-down-x current-mouse-state) current-x)
                                     (= (mouse-state-down-y current-mouse-state) current-y)))
                       (set-mouse-state-moving?! current-mouse-state #t))))
                 (send control on-event event)
                 (repaint!))
               (if (send event button-down? 'left)
                   (begin
                     (set-mouse-state-down-x! current-mouse-state (send event get-x))
                     (set-mouse-state-down-y! current-mouse-state (send event get-y))
                     (set-mouse-state-down?! current-mouse-state #t)
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

(generate-terrain/repaint 5 default-axis)
