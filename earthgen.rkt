#lang racket

(require racket/gui/base
         "flvector3.rkt"
         "quaternion.rkt"
         "planet.rkt"
         "planet-create.rkt"
         "climate-create.rkt"
         "grid.rkt"
         "color.rkt"
         "planet-color.rkt"
         "opengl.rkt"
         "projection.rkt"
         "math.rkt"
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
(define scale 0.9)
(define scale-max 100.0)
(define scale-min 0.5)

(define mouse-moving? #f)
(define mouse-down? #f)
(define mouse-down-x 0)
(define mouse-down-y 0)
(define mouse-down-latitude latitude)
(define mouse-down-longitude longitude)
(define milliseconds-between-frames 70.0)
(define last-draw (current-inexact-milliseconds))

(define planet-box (box #f))
(define planet-vector #f)
(define planet-vector-position #f)
(define grid-box (box (n-grid-list null 0)))
(define color-mode color-topography)
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

(define (->gl-vertex coord color)
  (make-gl-vertex
   (flvector-ref coord 0)
   (flvector-ref coord 1)
   (flvector-ref coord 2)
   (flcolor->byte (flcolor-red color))
   (flcolor->byte (flcolor-green color))
   (flcolor->byte (flcolor-blue color))
   0))

(define (make-tile-buffers!)
  (let* ([grid (first (unbox grid-box))]
         [tile-count (grid-tile-count grid)]
         [vertices (make-cvector _gl-vertex (* 7 tile-count))]
         [indices (make-cvector _uint (* 18 tile-count))]
         [color (flcolor 0.0 0.0 0.0)])
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

(define (set-gl-vertex-color! p color)
  (set-gl-vertex-red! p (byte-color-red color))
  (set-gl-vertex-green! p (byte-color-green color))
  (set-gl-vertex-blue! p (byte-color-blue color)))

(define (update-vertices! color-mode)
  (let* ([p (unbox planet-box)]
         [vertices (gl-buffer-data (get-gl-buffer 'tile-vertices))])
    (for ([n (tile-count p)])
      (let ([color (flcolor->byte-color (color-mode n))])
        (set-gl-vertex-color! (cvector-ref vertices (* n 7)) color)
        (for ([i 6])
          (set-gl-vertex-color! (cvector-ref vertices (+ 1 i (* n 7))) color))))
    (set-gl-vertex-buffer! 'tile-vertices vertices)))

(define (color-vector planet f)
  (let ([p (unbox planet-box)])
    (build-vector
     (vector-length (grid-tile-count (planet-grid p)))
     (lambda (n)
       (f p n)))))

(define frame
  (new frame%
       [label "earthgen"]
       [width window-width]
       [height window-height]))

(define frame-panel
  (new horizontal-panel%
       [parent frame]))

(define info-panel
  (new vertical-panel%
       [parent frame-panel]
       [min-width 400]
       [stretchable-width #f]))

(define (delete-children container)
  (map (lambda (c) (send container delete-child c))
       (send container get-children)))

(define canvas
  (new
   (class* canvas% ()
     (inherit with-gl-context swap-gl-buffers)
     (define/override (on-paint)
       (when (fl< milliseconds-between-frames
                  (fl- (current-inexact-milliseconds) last-draw))
         (thread
          (thunk
           (with-gl-context
            (thunk
             (let ([mx (fl* (fl/ 1.0 scale) (exact->inexact (/ display-width display-height)))]
                   [my (fl/ 1.0 scale)])
               (set-gl-ortho-projection (- mx) mx (- my) my -2.0 2.0))
             (for ([quat (list (list 90.0 -1.0 0.0 0.0)
                               (list (fl* (fl/ 180.0 pi) latitude) 1.0 0.0 0.0)
                               (list (fl* (fl/ 180.0 pi) longitude) 0.0 0.0 1.0))])
               (gl-rotate quat))
             (gl-clear)
             (gl-draw 'tile-vertices
                      'tile-indices)
             (gl-draw 'selected-tile-vertices
                      'selected-tile-indices)
             (swap-gl-buffers)))
           (set! last-draw (current-inexact-milliseconds))))))
     (define/override (on-size width height)
       (set! display-width width)
       (set! display-height height)
       (with-gl-context
        (thunk
         (set-gl-viewport 0 0 width height))))
     (define (repaint!)
       (set! last-draw 0.0)
       (on-paint))
     (define (color-planet! f)
       (when (planet? (unbox planet-box))
         (thread
          (thunk
           (set! color-mode f)
           (with-gl-context
            (thunk
             (update-vertices! (curry color-mode (unbox planet-box)))))
           (repaint!)))))
     (define (generate-terrain!)
       (thread
        (thunk
         (let-values ([(size method) (load "terrain-gen.rkt")])
           (with-gl-context
            (thunk
             (let ([grids (unbox grid-box)])
               (unless (= size (grid-subdivision-level (first grids)))
                 (set-box! grid-box (n-grid-list grids size))
                 (make-tile-buffers!)))))
           (let ([grids (n-grid-list (unbox grid-box) size)])
             (set-box! planet-box ((heightmap->planet (first grids)) (method grids))))
           (color-planet! color-mode)))))
     (define/override (on-char event)
       (define key-code (send event get-key-code))
       (match key-code
         ['escape (exit)]
         [#\q (generate-terrain!)]
         [#\w (let ([p (unbox planet-box)])
                (when (planet? p)
                  (thread
                   (thunk
                    (set-box! planet-box (climate-next (default-climate-parameters) p))
                    (color-planet! color-mode)))))]
         [#\e (begin
                (when (planet? (unbox planet-box))
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
                                                                  (list (climate-next par (unbox planet-box)))
                                                                  (cons (climate-next par (first ls)) ls)))))
                                           (reverse (planet-list 0 #f)))))
                    (set! planet-vector-position 0)
                    (set-box! planet-box (vector-ref planet-vector planet-vector-position))
                    (color-planet! color-mode)))))]
         ['left (begin
                  (when planet-vector
                    (set! planet-vector-position
                          (modulo (- planet-vector-position 1)
                                  (climate-parameters-seasons-per-cycle
                                   (planet-climate-parameters (unbox planet-box)))))
                    (set-box! planet-box (vector-ref planet-vector planet-vector-position))
                    (color-planet! color-mode)))]
         ['right (begin
                  (when planet-vector
                    (set! planet-vector-position
                          (modulo (+ planet-vector-position 1)
                                  (climate-parameters-seasons-per-cycle
                                   (planet-climate-parameters (unbox planet-box)))))
                    (set-box! planet-box (vector-ref planet-vector planet-vector-position))
                    (color-planet! color-mode)))]
         [#\a (color-planet! color-topography)]
         [#\s (color-planet! color-vegetation)]
         [#\d (color-planet! color-temperature)]
         [#\f (color-planet! color-humidity)]
         [#\l (color-planet! color-area)]
         ['wheel-up (begin
                      (set! scale
                            (min scale-max
                                 (* scale 1.05)))
                      (repaint!))]
         ['wheel-down (begin
                        (set! scale
                              (max scale-min
                                   (/ scale 1.05)))
                        (repaint!))]
         [_ (void)]))
     (define (tile-at x y)
       (if (unbox planet-box)
           (let ([mx (fl* 2.0 (fl* (fl* (fl/ 1.0 scale) (fl- (exact->inexact (/ x display-width)) 0.5)) (exact->inexact (/ display-width display-height))))]
                 [my (fl* -2.0 (fl/ (fl- (exact->inexact (/ y display-height)) 0.5) scale))])
             (if (< 1.0 (fl+ (flexpt mx 2.0) (flexpt my 2.0)))
                 #f
                 (let* ([p (unbox planet-box)]
                        [v (quaternion-vector-product (quaternion-inverse (rotation)) (orthographic->spherical mx my))]
                        [distance (build-flvector (tile-count p) (lambda (n) (flvector3-distance-squared v (tile-coordinates p n))))])
                   (foldl (lambda (n closest)
                            (if (< (flvector-ref distance n)
                                   (flvector-ref distance closest))
                                n
                                closest))
                          0
                          (range (tile-count p))))))
           #f))
     (define (update-info-panel tile)
       (delete-children info-panel)
       (when tile
         (let ([t (new text-field%
                      [label "tile id"]
                      [parent info-panel])])
           (send t set-value (number->string tile)))))
     (define/override (on-event event)
       (if (send event button-up? 'left)
           (begin
             (unless mouse-moving?
               (when (unbox planet-box)
                 (let ([tile (tile-at (send event get-x)
                                      (send event get-y))])
                   (update-info-panel tile)
                   (when tile
                     (let ([p (unbox planet-box)]
                           [vertices (gl-buffer-data (get-gl-buffer 'selected-tile-vertices))])
                       (begin
                         (cvector-set! vertices 0 (->gl-vertex (tile-coordinates p tile) (flcolor 1.0 0.0 0.0)))
                         (for ([n 6])
                           (cvector-set! vertices (+ 1 n) (->gl-vertex (corner-coordinates p (tile-corner p tile n)) (flcolor 1.0 0.0 0.0))))
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
                 (set! longitude
                       (fl+ mouse-down-longitude
                            (fl* (exact->inexact
                                  (- mouse-down-x (send event get-x)))
                                 (fl/ pi (* scale -900.0)))))
                 (set! latitude
                       (max (fl/ pi -2.0)
                            (min (fl/ pi 2.0)
                                 (fl+ mouse-down-latitude
                                      (fl* (exact->inexact
                                            (- mouse-down-y (send event get-y)))
                                           (fl/ pi (* scale -740.0)))))))
                 (repaint!))
               (if (send event button-down? 'left)
                   (begin
                     (set! mouse-down-x (send event get-x))
                     (set! mouse-down-y (send event get-y))
                     (set! mouse-down? true)
                     (set! mouse-down-latitude latitude)
                     (set! mouse-down-longitude longitude))
                   (void)))))
     (super-instantiate () (style '(gl))))
   [parent frame-panel]
   [min-width 400]
   [min-height 400]))

(define gl-context canvas)
(send canvas with-gl-context (thunk (set-gl-vertex-buffer! 'tile-vertices (make-cvector _gl-vertex 0))))
(send canvas with-gl-context (thunk (set-gl-index-buffer! 'tile-indices (make-cvector _uint 0))))
(send canvas with-gl-context (thunk (set-gl-vertex-buffer! 'selected-tile-vertices (make-cvector _gl-vertex 7))))
(send canvas with-gl-context (thunk (set-gl-index-buffer! 'selected-tile-indices
                                                          (let ([indices (make-cvector _uint 18)])
                                                            (for ([i 6])
                                                              (let ([k (+ (* i 3))])
                                                                (cvector-set! indices k 0)
                                                                (cvector-set! indices (+ 1 k) (+ 1 (modulo i 6)))
                                                                (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) 6)))))
                                                            indices))))
(send frame maximize #t)
(send frame show #t)
(send canvas focus)
