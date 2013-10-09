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
         "math.rkt"
         "color.rkt"
         "planet-color.rkt"
         math/flonum
         sgl/gl)

(define longitude 0.0)
(define latitude 0.0)
(define (rotation) (quaternion->matrix3
                    (quaternion*
                     (quaternion (fl/ pi 2.0) (vector3 1.0 0.0 0.0))
                     (quaternion latitude (vector3 -1.0 0.0 0.0))
                     (quaternion longitude (vector3 0.0 0.0 -1.0)))))
(define rotation-matrix (rotation))
(define scale 0.9)

(define mouse-down? false)
(define mouse-down-x 0)
(define mouse-down-y 0)
(define mouse-down-latitude latitude)
(define mouse-down-longitude longitude)
(define milliseconds-between-frames 20.0)
(define last-draw (current-inexact-milliseconds))

(define grids (n-grid-list null 0))

(define (terrain-gen) (let*-values ([(size method) (load "terrain-gen.txt")])
                        (begin
                          (set! grids (n-grid-list grids size))
                          (method grids))))

(define-values (display-width display-height) (get-display-size))
(define planet-entity #f)
(define tile-colors null)

(define (vector3->vertex v)
  (glVertex3d (flvector-ref v 0)
              (flvector-ref v 1)
              (flvector-ref v 2)))

(define (tile-vertices grid tile)
  (vector3->vertex (tile-coordinates tile))
  (for ([c (tile-corners tile)])
    (vector3->vertex (corner-coordinates (grid-corner grid c))))
  (vector3->vertex (corner-coordinates (grid-corner grid (tile-corner tile 0)))))

(define (set-gl-color! c)
  (glColor3f (color-red c)
             (color-green c)
             (color-blue c)))
  
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
        
        (if (planet? planet-entity)
            (let* ([grid (grid-list-first grids)]
                   [tiles (grid-tiles->vector grid)]
                   [corners (grid-corners->vector grid)])
              (for ([tile tiles])
                (glBegin GL_TRIANGLE_FAN)
                (set-gl-color! (vector-ref tile-colors (tile-id tile)))
                (tile-vertices grid tile)
                (glEnd)))
            (void))
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
   (vector-length (planet-tiles planet-entity))
   (lambda (n)
     (f
      (vector-ref (planet-tiles planet-entity) n)))))

(define canvas
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define/override (on-paint)
      (with-gl-context (lambda () (draw-opengl) (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context (lambda () (glViewport 0 0 width height))))
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (cond
        [(eq? #\q key-code)
         (begin
           (terrain-gen)
           (set! planet-entity (climate-first ((heightmap->planet (grid-list-first grids)) (terrain-gen)) (grid-list-first grids)))
           (if (planet? planet-entity)
               (set! tile-colors (color-vector planet-entity base-color))
               (void))
           (set! last-draw 0.0)
           (on-paint))]
        [(eq? #\a key-code)
         (when (planet? planet-entity)
           (begin
             (set! tile-colors (color-vector planet-entity base-color))
             (set! last-draw 0.0)
             (on-paint)))]
        [(eq? #\s key-code)
         (when (planet? planet-entity)
           (begin
             (set! tile-colors (color-vector planet-entity color-topography))
             (set! last-draw 0.0)
             (on-paint)))]
        [(eq? #\d key-code)
         (when (planet? planet-entity)
           (begin
             (set! tile-colors (color-vector planet-entity color-temperature))
             (set! last-draw 0.0)
             (on-paint)))]
        [(eq? #\d key-code)
         (void)]))
    (define/override (on-event event)
      (if (send event button-up? 'left)
          (set! mouse-down? false)
          (if mouse-down?
              (begin
                (set! longitude
                      (fl+ mouse-down-longitude
                           (fl* (exact->inexact
                                 (- mouse-down-x (send event get-x)))
                                (fl/ pi -800.0))))
                (set! latitude
                      (max (fl/ pi -2.0)
                           (min (fl/ pi 2.0)
                                (fl+ mouse-down-latitude
                                     (fl* (exact->inexact
                                           (- mouse-down-y (send event get-y)))
                                          (fl/ pi -600.0))))))
                (on-paint))
              (if (send event button-down? 'left)
                  (begin
                    (set! mouse-down? true)
                    (set! mouse-down-x (send event get-x))
                    (set! mouse-down-y (send event get-y))
                    (set! mouse-down-latitude latitude)
                    (set! mouse-down-longitude longitude))
                  (void)))))
    (super-instantiate () (style '(gl)))))

(define gl-context (new canvas [parent frame]))

(send frame maximize #t) 
(send frame show #t)