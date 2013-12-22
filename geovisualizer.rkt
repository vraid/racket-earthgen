#lang racket

(require racket/gui/base
         "dynamic-grid.rkt"
         "vector3.rkt"
         "quaternion.rkt"
         "matrix3.rkt"
         "logic.rkt"
         "color.rkt"
         math/flonum
         sgl/gl)

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

(define grid (top-grid))

(define-values
  (display-width display-height)
  (get-display-size))
(define planet-entity #f)

(define (flvector->vertex v)
  (glVertex3d (flvector-ref v 0)
              (flvector-ref v 1)
              (flvector-ref v 2)))

(define (tile-vertices tile)
  (flvector->vertex (tile-coordinates tile))
  (for ([c (tile-corners tile)])
    (flvector->vertex (corner-coordinates c)))
  (flvector->vertex (corner-coordinates (vector-ref (tile-corners tile) 0))))

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
        
        (for ([tile grid])
          (glBegin GL_TRIANGLE_FAN)
          (set-gl-color! (data-color ((tile-data tile))))
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
     (define/override (on-char event)
       (define key-code (send event get-key-code))
       (match key-code
         ['escape (exit)]
         [#\q (begin
                (set! grid (add-one-tile grid))
                (repaint!))]
         [#\w (begin
                (set! grid (push (closest-tile grid (flvector -0.5 0.75 0.75))))
                (repaint!))]
         [#\e (begin
                (set! grid (pop (closest-tile grid (flvector 0.0 1.0 0.0))))
                (repaint!))]
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