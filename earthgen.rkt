#lang racket

(require racket/gui/base
         "flvector3.rkt"
         "quaternion.rkt"
         (except-in "planet.rkt" planet)
         "planet-create.rkt"
         "climate-create.rkt"
         "color.rkt"
         "planet-color.rkt"
         "opengl.rkt"
         "projection.rkt"
         "sample-terrain.rkt"
         "gui/edit-panel.rkt"
         "control.rkt"
         "grid-handler.rkt"
         "if-let.rkt"
         math/flonum
         ffi/cvector
         ffi/unsafe)

(require plot
         profile
         profile/render-text)

(define longitude pi)
(define latitude 0.0)
(define (rotation) (quaternion-product
                    (axis-angle->quaternion (flvector 1.0 0.0 0.0) (fl/ pi 2.0))
                    (axis-angle->quaternion (flvector -1.0 0.0 0.0) latitude)
                    (axis-angle->quaternion (flvector 0.0 0.0 -1.0) longitude)))

(define mouse-moving? #f)
(define mouse-down? #f)
(define mouse-down-x 0)
(define mouse-down-y 0)
(define mouse-down-latitude latitude)
(define mouse-down-longitude longitude)
(define milliseconds-between-frames 70.0)
(define last-draw (current-inexact-milliseconds))

(define buffer-tile-count 0)
(define planet #f)
(define planet-vector #f)
(define planet-vector-position #f)
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
(require "logic.rkt"
         "heightmap-structs.rkt"
         "heightmap-create.rkt"
         "heightmap-functions.rkt")

(define (generate-terrain size method axis)
  (let ([grids (send grid-handler get-grids size)])
    (set! planet ((heightmap->planet (first grids)) (method grids) axis))))

(define (->gl-vertex coord color)
  (make-gl-vertex
   (flvector-ref coord 0)
   (flvector-ref coord 1)
   (flvector-ref coord 2)
   (flcolor->byte (flcolor-red color))
   (flcolor->byte (flcolor-green color))
   (flcolor->byte (flcolor-blue color))
   (flcolor->byte (flcolor-alpha color))))

(define (make-tile-buffers! grid)
  (let* ([tile-count (grid-tile-count grid)]
         [vertices (make-cvector _gl-vertex (* 7 tile-count))]
         [indices (make-cvector _uint (* 18 tile-count))]
         [color (flcolor3 0.0 0.0 0.0)])
    (set! buffer-tile-count tile-count)
    (for ([n tile-count])
      (cvector-set! vertices (* n 7) (->gl-vertex ((grid-tile-coordinates grid) n) color))
      (for ([i 6])
        (cvector-set! vertices (+ 1 i (* n 7)) (->gl-vertex ((grid-corner-coordinates grid) ((grid-tile-corner grid) n i))
                                                            color))
        (let ([k (+ (* i 3) (* n 18))])
          (cvector-set! indices k (* n 7))
          (cvector-set! indices (+ 1 k) (+ 1 (modulo i 6) (* n 7)))
          (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) 6) (* n 7))))))
    (set-gl-vertex-buffer! 'tile-vertices vertices)
    (set-gl-index-buffer! 'tile-indices indices)))

(define (set-vertex-color! vertices n color)
  (let ([v (cvector-ref vertices n)])
    (set-gl-vertex-red! v (byte-color-red color))
    (set-gl-vertex-green! v (byte-color-green color))
    (set-gl-vertex-blue! v (byte-color-blue color))))

(define (update-vertices! color-mode)
  (let* ([vertices (gl-buffer-data (get-gl-buffer 'tile-vertices))])
    (for ([n (tile-count planet)])
      (let ([color (flcolor->byte-color (color-mode n))])
        (set-vertex-color! vertices (* n 7) color)
        (for ([i 6])
          (set-vertex-color! vertices (+ 1 i (* n 7)) color))))
    (set-gl-vertex-buffer! 'tile-vertices vertices)))

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

(define global-panel
  (let* ([panel (new vertical-panel%
                     [parent no-frame]
                     [stretchable-height #f])]
         [p (edit-panel panel 30 100)]
         [size-edit (p "grid size")]
         [water-edit (p "water level")])
    (send size-edit
          link
          (thunk (number->string (grid-subdivision-level planet)))
          (lambda (size)
            (let ([size (string->number size)])
              (when (and (integer? size)
                         (<= 0 size))
                (thread
                 (thunk
                  (generate-terrain size (load "terrain-gen.rkt") default-axis)
                  (send canvas remake-mesh)
                  (color-planet! color-mode)))))))
    (send water-edit
          link
          (thunk (number->string (planet-sea-level planet)))
          (lambda (level)
            (let ([level (string->number level)])
              (when (real? level)
                (let ([level (fl level)])
                  (set-planet-sea-level! planet level)
                  (for ([n (tile-count planet)])
                    ((tile-data-water-level-set! (planet-tile planet)) n level))
                  (color-planet! color-mode))))))
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
      (send climate-panel show (and tile (planet-has-climate? planet)))
      (when tile
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
                  (thunk* #f))))))))

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

(define (color-planet! f)
  (when planet
    (thread
     (thunk
      (set! color-mode f)
      (send canvas with-gl-context
            (thunk
             (update-vertices! (curry color-mode planet))))
      (repaint!)))))

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
       (when (fl< milliseconds-between-frames
                  (fl- (current-inexact-milliseconds) last-draw))
         (thread
          (thunk
           (with-gl-context
            (thunk
             (send control set-projection)
             (gl-rotate (send control rotation-list planet))
             (gl-clear (list 0.0 0.0 0.0 0.0))
             (gl-cull-face 'back)
             (gl-draw 'tile-vertices
                      'tile-indices)
             #;(gl-draw 'selected-tile-vertices
                        'selected-tile-indices)
             (swap-gl-buffers)))
           (set! last-draw (current-inexact-milliseconds))))))
     (define/override (on-size width height)
       (send control resize-viewport width height)
       (set! display-width width)
       (set! display-height height)
       (with-gl-context
        (thunk
         (set-gl-viewport 0 0 width height))))
     (define/public (remake-mesh)
       (with-gl-context
          (thunk
           (unless (= buffer-tile-count (tile-count planet))
             (make-tile-buffers! planet)))))
     (define (generate-terrain!)
       (thread
        (thunk
         (generate-terrain (grid-subdivision-level planet) (load "terrain-gen.rkt") default-axis)
         (remake-mesh)
         (color-planet! color-mode))))
     (define/override (on-char event)
       (define key-code (send event get-key-code))
       (match key-code
         ['escape (exit)]
         [#\q (generate-terrain!)]
         [#\w (when planet
                (thread
                 (thunk
                  (set! planet-vector #f)
                  (set! planet (climate-next (default-climate-parameters) planet))
                  (color-planet! color-mode))))]
         [#\e (begin
                (when planet
                  (thread
                   (thunk
                    (set! planet-vector (list->vector
                                         (let* ([par (default-climate-parameters)]
                                                [seasons (climate-parameters-seasons-per-cycle par)])
                                           (define (planet-list n ls)
                                             (if (= seasons n)
                                                 ls
                                                 (planet-list (+ 1 n) 
                                                              (if (zero? n)
                                                                  (list (climate-next par planet))
                                                                  (cons (climate-next par (first ls)) ls)))))
                                           (reverse (planet-list 0 #f)))))
                    (set! planet-vector-position 0)
                    (set! planet (vector-ref planet-vector planet-vector-position))
                    (color-planet! color-mode)))))]
         #;[#\t (when (and planet (planet-has-climate? planet))
                (set! planet (next-turn default-turn-parameters planet))
                (color-planet! color-mode))]
         ['left (begin
                  (when planet-vector
                    (set! planet-vector-position
                          (modulo (- planet-vector-position 1)
                                  (climate-parameters-seasons-per-cycle
                                   (planet-climate-parameters planet))))
                    (set! planet (vector-ref planet-vector planet-vector-position))
                    (color-planet! color-mode)))]
         ['right (begin
                   (when planet-vector
                     (set! planet-vector-position
                           (modulo (+ planet-vector-position 1)
                                   (climate-parameters-seasons-per-cycle
                                    (planet-climate-parameters planet))))
                     (set! planet (vector-ref planet-vector planet-vector-position))
                     (color-planet! color-mode)))]
         [#\a (color-planet! color-topography)]
         [#\s (color-planet! color-vegetation)]
         [#\d (color-planet! color-temperature)]
         [#\f (color-planet! color-humidity)]
         [#\g (color-planet! color-aridity)]
         [#\l (color-planet! (color-area
                              (stream-fold (lambda (a n)
                                             (max a (tile-area planet n)))
                                           0.0
                                           (in-range (tile-count planet)))))]
         ['wheel-up (begin
                      (send control wheel-up)
                      (repaint!))]
         ['wheel-down (begin
                        (send control wheel-down)
                        (repaint!))]
         [_ (void)]))
     (define (tile-at x y)
       (if planet
           (and-let* ([v (send control get-coordinates planet x y)]
                      [distance (build-flvector (tile-count planet) (lambda (n) (flvector3-distance-squared v (tile-coordinates planet n))))])
                     (foldl (lambda (n closest)
                              (if (< (flvector-ref distance n)
                                     (flvector-ref distance closest))
                                  n
                                  closest))
                            0
                            (range (tile-count planet))))
           #f))
     
     (define/override (on-event event)
       (if (send event button-up? 'left)
           (begin
             (unless mouse-moving?
               (when planet
                 (let ([tile (tile-at (send event get-x)
                                      (send event get-y))])
                   (update-tile-panel tile)
                   (when tile
                     (let ([vertices (gl-buffer-data (get-gl-buffer 'selected-tile-vertices))])
                       (begin
                         (cvector-set! vertices 0 (->gl-vertex (tile-coordinates planet tile) (flcolor3 1.0 0.0 0.0)))
                         (for ([n 6])
                           (cvector-set! vertices (+ 1 n) (->gl-vertex (corner-coordinates planet (tile-corner planet tile n)) (flcolor3 1.0 0.0 0.0))))
                         (with-gl-context
                          (thunk
                           (set-gl-vertex-buffer! 'selected-tile-vertices vertices))))))))
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
                     (set! mouse-down-latitude latitude)
                     (set! mouse-down-longitude longitude)
                     (send control on-event event))
                   (void)))))
     (super-instantiate () (style '(gl))))
   [parent frame-panel]
   [min-width 400]
   [min-height 400]))

(define gl-context canvas)
(send canvas with-gl-context (thunk 
                              (make-tile-buffers! (send grid-handler get-grid default-grid-size))
                              (update-vertices! (curry color-mode planet))))

(send canvas with-gl-context (thunk (set-gl-vertex-buffer! 'selected-tile-vertices
                                                           (let ([vectors (make-cvector _gl-vertex 7)]
                                                                 [zero-vertex (->gl-vertex (flvector 0.0 0.0 0.0)
                                                                                           (flcolor3 0.0 0.0 0.0))])
                                                             (for ([i 7])
                                                               (cvector-set! vectors i zero-vertex))
                                                             vectors))))
(send canvas with-gl-context (thunk (set-gl-index-buffer! 'selected-tile-indices
                                                          (let ([indices (make-cvector _uint 18)])
                                                            (for ([i 6])
                                                              (let ([k (+ (* i 3))])
                                                                (cvector-set! indices k 0)
                                                                (cvector-set! indices (+ 1 k) (+ 1 (modulo i 6)))
                                                                (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) 6)))))
                                                            indices))))

(init-info-panel)
(send frame maximize #t)
(send frame show #t)
(send canvas focus)
(send canvas on-paint)
