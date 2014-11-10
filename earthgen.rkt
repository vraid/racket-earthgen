#lang racket

(require racket/gui/base
         vraid/math
         vraid/flow
         vraid/opengl
         "planet/planet.rkt"
         "planet/planet-create.rkt"
         "planet/climate-create.rkt"
         "planet-color.rkt"
         "sample-terrain.rkt"
         "gui/edit-panel.rkt"
         "interface/control.rkt"
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

(define planet-handler (new planet-handler%
                            [max-elements 24]))
(define grid-handler (new-grid-handler))
(define default-grid-size 5)
(define color-mode color-vegetation)
(define selected-tile #f)

(define-values
  (display-width display-height)
  (get-display-size))

(define-values
  (window-width window-height)
  (values 800 600))

; required by terrain-gen
(require "planet/heightmap/heightmap-structs.rkt"
         "planet/heightmap/heightmap-create.rkt"
         "planet/heightmap/heightmap-functions.rkt")

(define (generate-terrain size method axis)
  (let ([grids (send grid-handler get-grids size)])
    (send planet-handler
          terrain/scratch
          (thunk ((heightmap->planet (first grids)) (method grids) axis)))
    (void)))

(generate-terrain default-grid-size sample-terrain default-axis)

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

(define info-panel
  (new (class* tab-panel% ()
         (super-instantiate ())
         (define/public (set-tab selection)
           (for ([n (vector-length info-panel-choices)])
             (send (tab-choice-panel (vector-ref info-panel-choices n))
                   reparent
                   (if (= n selection)
                       info-panel
                       no-frame)))))
       
       [choices (list)]
       [parent frame-panel]
       [min-width 300]
       [stretchable-width #f]
       [callback (lambda (panel event)
                   (send panel set-tab (send panel get-selection)))]))

(define (color-planet! color-function)
  (send canvas with-gl-context
        (thunk (send planet-renderer set-tile-colors color-function)))
  (repaint!))

(define global-panel
  (let* ([panel (new vertical-panel%
                     [parent no-frame]
                     [stretchable-height #f])]
         [p (edit-panel panel 30 100)]
         [size-edit (p "grid size")]
         [water-edit (p "water level")])
    (send size-edit
          link
          (thunk (number->string (grid-subdivision-level (send planet-handler current))))
          (lambda (size)
            (let ([size (string->number size)])
              (when (and (integer? size)
                         (<= 0 size))
                (thread
                 (thunk
                  (generate-terrain size (load "terrain-gen.rkt") default-axis)
                  (send canvas with-gl-context (thunk (send planet-renderer update/planet)))
                  (color-planet! color-mode)))))))
    (send water-edit
          link
          (thunk (number->string (planet-sea-level (send planet-handler current))))
          (lambda (level)
            (let ([planet (send planet-handler current)])
              (let ([level (string->number level)])
                (when (real? level)
                  (let ([level (fl level)])
                    (set-planet-sea-level! planet level)
                    (for ([n (tile-count planet)])
                      ((tile-data-water-level-set! (planet-tile planet)) n level))
                    (color-planet! color-mode)))))))
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
         [climate-panel (new vertical-panel%
                             [parent tile-panel]
                             [stretchable-height #f])]
         [climate (edit-panel climate-panel height label-width)]
         [id-edit (general "tile id")]
         [elevation-edit (general "elevation")]
         [water-level-edit (general "water level")]
         [temperature-edit (climate "temperature")]
         [absolute-humidity-edit (climate "absolute humidity")]
         [relative-humidity-edit (climate "relative humidity")]
         [precipitation-edit (climate "precipitation")]
         [vegetation-edit (climate "vegetation")])
    (lambda (tile)
      (send general-panel show tile)
      (send climate-panel show (and tile (planet-has-climate? (send planet-handler current))))
      (when tile
        (let ([planet (send planet-handler current)])
          (let ([link (lambda (panel get set) (send panel link get set))])
            (link id-edit (thunk (number->string tile)) (thunk* #f))
            (link elevation-edit
                  (thunk (number->string (tile-elevation planet tile)))
                  (lambda (n)
                    (let ([num (exact->inexact (string->number n))])
                      (when (real? num)
                        (begin
                          ((tile-data-elevation-set! (planet-tile planet)) tile num)
                          (color-planet! color-mode))))))
            (link water-level-edit
                  (thunk (number->string (tile-water-level planet tile)))
                  (lambda (n)
                    (let ([num (exact->inexact (string->number n))])
                      (when (real? num)
                        (begin
                          ((tile-data-water-level-set! (planet-tile planet)) tile num)
                          (color-planet! color-mode))))))
            (when (planet-has-climate? planet)
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
              (link vegetation-edit
                    (thunk (number->string (tile-vegetation planet tile)))
                    (thunk* #f)))))))))

(struct tab-choice
  (label panel))

(define info-panel-choices
  (vector (tab-choice "global" global-panel)
          (tab-choice "tile" tile-panel)))

(define (init-info-panel)
  (send info-panel set (map tab-choice-label (vector->list info-panel-choices)))
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
         (generate-terrain (grid-subdivision-level (send planet-handler current)) (load "terrain-gen.rkt") default-axis)
         (with-gl-context (thunk (send planet-renderer update/planet)))
         (color-planet! color-mode))))
     (define/override (on-char event)
       (define key-code (send event get-key-code))
       (match key-code
         ['escape (exit)]
         [#\q (generate-terrain!)]
         [#\w (and-let ([planet (send planet-handler current)])
                (thread
                 (thunk
                  (send planet-handler
                        climate/scratch
                        (curry climate-next (default-climate-parameters)))
                  (with-gl-context (thunk (send planet-renderer update/planet)))
                  (color-planet! color-mode))))]
         [#\e (and-let ([planet (send planet-handler current)])
                (thread
                 (thunk
                  (send planet-handler
                        climate/add
                        (let* ([par (default-climate-parameters)]
                               [seasons (climate-parameters-seasons-per-cycle par)])
                          (curry climate-next par)))
                  (with-gl-context (thunk (send planet-renderer update/planet)))
                  (color-planet! color-mode))))]
         [#\t (and-let ([planet (send planet-handler current)])
                (thread
                 (thunk
                  (for ([n 23])
                    (displayln n)
                    (send planet-handler
                          climate/add
                          (let* ([par (default-climate-parameters)]
                                 [seasons (climate-parameters-seasons-per-cycle par)])
                            (curry climate-next par))))
                  (with-gl-context (thunk (send planet-renderer update/planet)))
                  (color-planet! color-mode))))]
         #;[#\t (when (and planet (planet-has-climate? planet))
                  (set! planet (next-turn default-turn-parameters planet))
                  (color-planet! color-mode))]
         ['left (when (send planet-handler earlier)
                  (with-gl-context (thunk (send planet-renderer update/planet)))
                  (color-planet! color-mode))]
         ['right (when (send planet-handler later)
                   (with-gl-context (thunk (send planet-renderer update/planet)))
                   (color-planet! color-mode))]
         [#\a (color-planet! color-topography)]
         [#\s (color-planet! color-vegetation)]
         [#\d (color-planet! color-temperature)]
         [#\f (color-planet! color-humidity)]
         [#\g (color-planet! color-aridity)]
         [#\h (color-planet! color-precipitation)]
         [#\l (color-planet! (color-area
                              (let ([planet (send planet-handler current)])
                                (stream-fold (lambda (a n)
                                               (max a (tile-area planet n)))
                                             0.0
                                             (in-range (tile-count planet))))))]
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

;(color-planet! color-vegetation)
