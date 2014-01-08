#lang racket

(require racket/gui/base
         "logic.rkt"
         "vector3.rkt"
         "quaternion.rkt"
         "matrix3.rkt"
         "heightmap-structs.rkt"
         "heightmap-create.rkt"
         "heightmap-functions.rkt"
         "planet.rkt"
         "planet-create.rkt"
         "climate-create.rkt"
         "grid.rkt"
         "color.rkt"
         "planet-color.rkt"
         "draw-structs.rkt"
         "opengl.rkt"
         math/flonum
         ffi/cvector
         ffi/unsafe)

(define longitude pi)
(define latitude 0.0)
(define (rotation) (quaternion*
                    (angle-axis->quaternion (fl/ pi 2.0) (flvector 1.0 0.0 0.0))
                    (angle-axis->quaternion latitude (flvector -1.0 0.0 0.0))
                    (angle-axis->quaternion longitude (flvector 0.0 0.0 -1.0))))
(define scale 0.9)
(define scale-max 100.0)
(define scale-min 0.5)

(define mouse-down? false)
(define mouse-down-x 0)
(define mouse-down-y 0)
(define mouse-down-latitude latitude)
(define mouse-down-longitude longitude)
(define milliseconds-between-frames 70.0)
(define last-draw (current-inexact-milliseconds))

(define grids (n-grid-list null 0))
(define draw-tiles (vector))
(define color-mode color-topography)

(define-values
  (display-width display-height)
  (get-display-size))
(define planet-entity #f)

(define (terrain-gen)
  (begin
    (define-values (size method) (load "terrain-gen.rkt"))
    (set! grids (n-grid-list grids size))
    (set! draw-tiles
          (vector-map
           (lambda (tile)
             (draw-tile
              (flcolor 0.0 0.0 0.0)
              (tile-coordinates tile)
              (build-vector 6
                            (lambda (n)
                              (corner-coordinates (grid-corner (first grids) (tile-corner tile n)))))))
           (grid-tiles->vector (first grids))))
    (method grids)))

(define (color->byte c)
  (max 0
       (min 255
            (inexact->exact
             (round (fl* 255.0 c))))))

(define (->gl-vertex coord color)
  (make-gl-vertex
   (flvector-ref coord 0)
   (flvector-ref coord 1)
   (flvector-ref coord 2)
   (color->byte (flcolor-red color))
   (color->byte (flcolor-green color))
   (color->byte (flcolor-blue color))
   0))

(define (make-vertices!)
  (let* ([grid (first grids)]
         [vertices (make-cvector _gl-vertex (* 7 (grid-tile-count grid)))]
         [indices (make-cvector _uint (* 18 (grid-tile-count grid)))])
    (begin
      (for ([n (grid-tile-count grid)]
            [tile (grid-tiles grid)])
        (begin
          (let ([color (draw-tile-color (vector-ref draw-tiles (tile-id tile)))])
            (cvector-set! vertices (* n 7) (->gl-vertex (tile-coordinates tile) color))
            (for ([i 6])
              (cvector-set! vertices (+ 1 i (* n 7)) (->gl-vertex (corner-coordinates (grid-corner grid (tile-corner tile i))) color))
              (let ([k (+ (* i 3) (* n 18))])
                (cvector-set! indices k (* n 7))
                (cvector-set! indices (+ 1 k) (+ 1 (modulo i 6) (* n 7)))
                (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) 6) (* n 7))))))))
      (set-gl-vertex-data vertices)
      (set-gl-index-data indices))))

(define frame
  (new frame%
       [label "earthgen"]
       [width display-width]
       [height display-height]
       [style '(no-resize-border
                no-caption
                no-system-menu)]))

(define (color-vector planet f)
  (build-vector
   (vector-length (grid-tile-count (planet-grid planet-entity)))
   (lambda (n)
     (f planet-entity n))))

(define (tile-longitude tile-id grid)
  (let* ([t (grid-tile grid tile-id)]
         [coord (tile-coordinates t)]
         [x (flvector-ref coord 0)]
         [y (flvector-ref coord 1)])
    (atan y x)))

(define canvas
  (new
   (class* canvas% ()
     (inherit with-gl-context swap-gl-buffers)
     (define/override (on-paint)
       (when (fl< milliseconds-between-frames
                  (fl- (current-inexact-milliseconds) last-draw))
         (begin
           (with-gl-context
            (lambda ()
              (begin
                (let ([mx (fl* (fl/ 1.0 scale) (exact->inexact (/ display-width display-height)))]
                      [my (fl/ 1.0 scale)])
                  (set-gl-ortho-projection (- mx) mx (- my) my -2.0 2.0))
                (for ([quat (list (list 90.0 -1.0 0.0 0.0)
                                  (list (fl* (fl/ 180.0 pi) latitude) 1.0 0.0 0.0)
                                  (list (fl* (fl/ 180.0 pi) longitude) 0.0 0.0 1.0))])
                  (rotate-gl quat))
                (draw-gl) (swap-gl-buffers))))
           (set! last-draw (current-inexact-milliseconds)))))
     (define/override (on-size width height)
       (begin
         (set! display-width width)
         (set! display-height height)
         (with-gl-context
          (lambda ()
            (set-gl-viewport 0 0 width height)))))
     (define (repaint!)
       (set! last-draw 0.0)
       (on-paint))
     (define (color-planet! planet-entity f)
       (when (planet? planet-entity)
         (begin
           (set! color-mode f)
           (for ([n (tile-count planet-entity)])
             (let ([d-tile (vector-ref draw-tiles n)])
               (set-draw-tile-color! d-tile (f planet-entity n))))
           (with-gl-context make-vertices!)
           (repaint!))))
     (define (generate-terrain!)
       (begin
         (thread
          (lambda ()
            (terrain-gen)
            (set! planet-entity ((heightmap->planet (first grids)) (terrain-gen)))
            (color-planet! planet-entity
                           color-mode)))))
     (define/override (on-char event)
       (define key-code (send event get-key-code))
       (match key-code
         ['escape (exit)]
         [#\q (generate-terrain!)]
         [#\w (begin
                (set! planet-entity (climate-next (climate-parameters) planet-entity))
                (color-planet! planet-entity
                               color-mode)
                (repaint!))]
         [#\a (color-planet! planet-entity
                             base-color)]
         [#\s (color-planet! planet-entity
                             color-topography)]
         [#\d (color-planet! planet-entity
                             color-temperature)]
         [#\f (color-planet! planet-entity
                             color-albedo)]
         ['wheel-up (begin
                      (set! scale
                            (min scale-max
                                 (* scale 1.05)))
                      (on-paint))]
         ['wheel-down (begin
                        (set! scale
                              (max scale-min
                                   (/ scale 1.05)))
                        (on-paint))]
         [_ (void)]))
     (define/override (on-event event)
       (if (send event button-up? 'left)
           (set! mouse-down? false)
           (if mouse-down?
               (begin
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
                 (on-paint))
               (if (send event button-down? 'left)
                   (begin
                     (set! mouse-down? true)
                     (set! mouse-down-x (send event get-x))
                     (set! mouse-down-y (send event get-y))
                     (set! mouse-down-latitude latitude)
                     (set! mouse-down-longitude longitude))
                   (void)))))
     (super-instantiate () (style '(gl))))
   [parent frame]))

(define gl-context canvas)

(send frame maximize #t) 
(send frame show #t)
(send canvas focus)
(send canvas with-gl-context (lambda () (set-gl-vertex-data (make-cvector _gl-vertex 0))))
(send canvas with-gl-context (lambda () (set-gl-index-data (make-cvector _uint 0))))
