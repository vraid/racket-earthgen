#lang racket

(require racket/gui/base
         "heightmap-structs.rkt"
         "heightmap-create.rkt"
         "heightmap-functions.rkt"
         "planet.rkt"
         "planet-create.rkt"
         "climate-create.rkt"
         "grid.rkt"
         "vector3.rkt"
         "quaternion.rkt"
         "matrix3.rkt"
         "logic.rkt"
         "color.rkt"
         "planet-color.rkt"
         "draw-structs.rkt"
         "load-image.rkt"
         "projection.rkt"
         math/flonum
         sgl/gl)

(define image-path "C:/directory/image.ext")
(define longitude pi)
(define latitude 0.0)
(define (rotation) (quaternion->matrix3
                    (quaternion*
                     (angle-axis->quaternion (fl/ pi 2.0) (flvector 1.0 0.0 0.0))
                     (angle-axis->quaternion latitude (flvector -1.0 0.0 0.0))
                     (angle-axis->quaternion longitude (flvector 0.0 0.0 -1.0)))))
(define rotation-matrix (rotation))
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

(define-values
  (display-width display-height)
  (get-display-size))
(define planet-entity #f)

(define (flvector->vertex v)
  (glVertex3d (flvector-ref v 0)
              (flvector-ref v 1)
              (flvector-ref v 2)))

(define (tile-vertices tile)
  (flvector->vertex (draw-tile-center tile))
  (for ([c (draw-tile-corners tile)])
    (flvector->vertex c))
  (flvector->vertex (vector-ref (draw-tile-corners tile) 0)))

(define (set-gl-color! c)
  (glColor3f (flcolor-red c)
             (flcolor-green c)
             (flcolor-blue c)))

(define (draw-opengl)
  (if (fl< milliseconds-between-frames (fl- (current-inexact-milliseconds) last-draw))
      (begin
        (glFrontFace GL_CCW)
        (glEnable GL_CULL_FACE)
        (glCullFace GL_BACK)
        (glClearColor 0.0 0.0 0.0 0.0)
        (glClear GL_COLOR_BUFFER_BIT)
        
        (glShadeModel GL_SMOOTH)
        
        (glMatrixMode GL_PROJECTION)
        (glLoadIdentity)
        (let ([mx (fl* (fl/ 1.0 scale) (exact->inexact (/ display-width display-height)))]
              [my (fl/ 1.0 scale)])
          (glOrtho (- mx) mx (- my) my -2.0 2.0))
        (glRotatef 90.0 -1.0 0.0 0.0)
        (glRotatef (fl* (fl/ 180.0 pi) latitude) 1.0 0.0 0.0)
        (glRotatef (fl* (fl/ 180.0 pi) longitude) 0.0 0.0 1.0)
        
            (for ([tile draw-tiles])
              (glBegin GL_TRIANGLE_FAN)
              (set-gl-color! (draw-tile-color tile))
              (tile-vertices tile)
              (glEnd))
            (void)
        (set! last-draw (current-inexact-milliseconds)))
      (void)))

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
       (with-gl-context (lambda () (draw-opengl) (swap-gl-buffers))))
     (define/override (on-size width height)
       (begin
         (set! display-width width)
         (set! display-height height)
         (with-gl-context (lambda () (glViewport 0 0 width height)))))
     (define (repaint!)
       (set! last-draw 0.0)
       (on-paint))
     (define (color-planet! planet-entity f)
       (when (planet? planet-entity)
         (begin
           (for ([n (tile-count planet-entity)])
             (let ([d-tile (vector-ref draw-tiles n)])
               (set-draw-tile-color! d-tile (f planet-entity n))))
           (repaint!))))
     (define (generate-terrain!)
       (begin
         (thread
          (lambda ()
            (terrain-gen)
            (set! planet-entity ((heightmap->planet (first grids)) (terrain-gen)))
            (color-planet! planet-entity
                           color-topography)))))
     (define/override (on-char event)
       (define key-code (send event get-key-code))
       (match key-code
         ['escape (exit)]
         [#\q (generate-terrain!)]
         [#\w (begin
                (set! planet-entity (climate-next (climate-parameters) planet-entity))
                (repaint!))]
         [#\e (color-planet! planet-entity
                             ((lambda ()
                                (let* ([image (load-image/file image-path)]
                                       [width (image-width image)]
                                       [height (image-height image)]
                                       [rel->rect (relative->rectangular width height)]
                                       [pixel-color (pixel-color image)])
                                  (lambda (tile)
                                    (let* ([lat (tile-latitude tile (planet-grid planet-entity))]
                                           [lon (tile-longitude tile (planet-grid planet-entity))]
                                           [coord (equirectangular-projection lon lat)]
                                           [px (rel->rect coord)]
                                           [x (vector-ref px 0)]
                                           [y (vector-ref px 1)])
                                      (pixel-color x y)
                                      ))))))]
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