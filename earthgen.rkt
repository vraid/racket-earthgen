#lang racket
(require racket/gui/base
         "heightmap-structs.rkt"
         "heightmap-create.rkt"
         "heightmap-functions.rkt"
         "grid.rkt"
         "vector3.rkt"
         "quaternion.rkt"
         "matrix3.rkt"
         "logic.rkt"
         "math.rkt"
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

(define-values (display-width display-height) (get-display-size))
(define planet (let* ([grids (n-grid-list 6)]
                      [grid (force (grid-list-first (force grids)))]
                      [continent (heightmap-lower
                                  500.0 (heightmap-create
                                         (heightmap-parameters "earth 5" 2 2000.0 0.65)))]
                      [mountain-base (heightmap-lower
                                      200.0
                                      (heightmap-create
                                       (heightmap-parameters "mtn 7" 5 2500.0 0.7)))]
                      [mountain (heightmap-map
                                 (lambda (a b . ns)
                                   (if (both true?
                                             (fl< -200.0 a)
                                             (fl< 0.0 b))
                                       b
                                       0.0))
                                 continent
                                 mountain-base)]
                                             
                      [final-terrain (heightmap-combine continent
                                                        mountain)])
                 (list grid (final-terrain grids))))

(define (vector3->vertex v)
  (glVertex3d (flvector-ref v 0)
              (flvector-ref v 1)
              (flvector-ref v 2)))
  
(define (tile-vertices grid tile)
  (vector3->vertex (tile-coordinates tile))
  (for ([c (tile-corners tile)])
    (vector3->vertex (corner-coordinates (grid-corner grid c))))
  (vector3->vertex (corner-coordinates (grid-corner grid (tile-corner tile 0)))))

(define (vector3->color v)
  (glColor3f (flvector-ref v 0)
             (flvector-ref v 1)
             (flvector-ref v 2)))

(define (color-interpolate col-one col-two d)
  (vector3+ (vector3-scale col-one (- 1.0 d)) (vector3-scale col-two d)))

(define water-surface
  (vector3 0.0 0.4 0.8))
(define water-deep
  (vector3 0.0 0.0 0.3))
(define land-low
  (vector3 0.5 0.8 0.0))
(define land-high
  (vector3 0.2 0.2 0.1))

(define (tile-color elevation)
  (let ([col (if (< 0 elevation)
                 (color-interpolate land-low land-high (exact->inexact (min 1.0 (/ elevation 3000.0))))
                 (color-interpolate water-surface water-deep (exact->inexact (min 1.0 (/ elevation -3000.0)))))])
    (vector3->color col)))

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
        
        (let* ([grid (first planet)]
               [terrain (second planet)]
               [tiles (grid-tiles->vector grid)]
               [corners (grid-corners->vector grid)])
          (for ([tile tiles])
            (glBegin GL_TRIANGLE_FAN)
            (tile-color (flvector-ref (heightmap-tiles terrain) (tile-id tile)))
            (tile-vertices grid tile)
            (glEnd)))
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

(define canvas
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define/override (on-paint)
      (with-gl-context (lambda () (draw-opengl) (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context (lambda () (glViewport 0 0 width height))))
    (define/override (on-event event)
      (if (send event button-up? 'left)
          (set! mouse-down? false)
          (if mouse-down?
              (begin
                (set! longitude (fl+ mouse-down-longitude
                                     (fl* (exact->inexact
                                           (- mouse-down-x (send event get-x)))
                                          (fl/ pi -800.0))))
                (set! latitude (max (fl/ pi -2.0)
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