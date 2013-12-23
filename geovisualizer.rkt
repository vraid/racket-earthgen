#lang racket

(require racket/gui/base
         "dynamic-grid.rkt"
         "quaternion.rkt"
         "matrix3.rkt"
         math/flonum
         ffi/vector
         ffi/cvector
         ffi/unsafe
         (planet stephanh/RacketGL:1:4/rgl))

(define-cstruct _vertex
  ([x _float]
   [y _float]
   [z _float]
   [red _byte]
   [green _byte]
   [blue _byte]
   [alpha _byte]))

(define uint-size 4)
(define byte-size 1)
(define float-size 4)
(define vertex-size (+ (* 3 float-size) (* 4 byte-size)))

(define longitude pi)
(define latitude 0.0)
(define (rotation)
  (quaternion->matrix3
   (quaternion*
    (angle-axis->quaternion (fl/ pi 2.0) (flvector 1.0 0.0 0.0))
    (angle-axis->quaternion latitude (flvector -1.0 0.0 0.0))
    (angle-axis->quaternion longitude (flvector 0.0 0.0 -1.0)))))
(define (inverse-rotation)
  (quaternion->matrix3
   (quaternion-inverse
    (quaternion*
     (angle-axis->quaternion (fl/ pi 2.0) (flvector 1.0 0.0 0.0))
     (angle-axis->quaternion latitude (flvector -1.0 0.0 0.0))
     (angle-axis->quaternion longitude (flvector 0.0 0.0 -1.0))))))
(define rotation-matrix (rotation))
(define scale 0.9)

(define (subdivision-level-tile-count n)
  (+ 2 (* 10 (expt 3 n))))

(define base-level 5)
(define level (+ base-level 0))

(define grid (top-grid))

(define (set-scale!)
  (set! scale (* 0.9 (sqrt (/ (subdivision-level-tile-count (max base-level
                                                                level))
                            (subdivision-level-tile-count base-level))))))

(define (set-level! n)
  (begin
    (set! level (max base-level n))
    (set-scale!)))

(define vertices #f)
(define indices #f)

(define vertex-buffer #f)
(define index-buffer #f)

(define (get-buffer-name)
  (let ((buffer (glGenBuffers 1)))
    (u32vector-ref buffer 0)))

(define (init-gl)  
  (set! vertex-buffer (get-buffer-name))
  (glBindBuffer GL_ARRAY_BUFFER vertex-buffer)
  (glBufferData GL_ARRAY_BUFFER (* vertex-size (cvector-length vertices)) (cvector-ptr vertices) GL_STATIC_DRAW)
  (glBindBuffer GL_ARRAY_BUFFER 0)

  (set! index-buffer (get-buffer-name))
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER index-buffer)
  (glBufferData GL_ELEMENT_ARRAY_BUFFER (* uint-size (cvector-length indices)) (cvector-ptr indices) GL_DYNAMIC_DRAW)
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0)

  (glClearColor 1.0 1.0 1.0 1.0)
  )  

(define (->vertex coord color)
  (make-vertex
   (flvector-ref coord 0)
   (flvector-ref coord 1)
   (flvector-ref coord 2)
   (bytes-ref color 1)
   (bytes-ref color 2)
   (bytes-ref color 3)
   (bytes-ref color 0)))

(define (make-vertices!)
  (begin
    (set! vertices (make-cvector _vertex (* 7 (set-count grid))))
    (set! indices (make-cvector _uint (* 18 (set-count grid))))
    (for ([n (set-count grid)]
          [tile (in-set grid)])
      (begin
        (cvector-set! vertices (* n 7) (->vertex (tile-coordinates tile) (tile-color tile)))
        (for ([i 6])
          (cvector-set! vertices (+ 1 i (* n 7)) (->vertex (corner-coordinates (tile-corner tile i)) (tile-color tile)))
          (let ([k (+ (* i 3) (* n 18))])
            (cvector-set! indices k (* n 7))
            (cvector-set! indices (+ 1 k) (+ 1 (modulo i 6) (* n 7)))
            (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) 6) (* n 7)))))))))

(define (make-grid!)
  (let* ([base-radius (sqrt 3.2)]
         [radius (* base-radius
                    (sqrt (/ (subdivision-level-tile-count base-level)
                             (subdivision-level-tile-count (max base-level
                                                                level)))))]
         [v (matrix3-vector3* (inverse-rotation) (flvector 0.0 0.0 1.0))])
    (begin
      (set! grid (expand v (min (sqrt 2.0) radius) (foldl (lambda (n t)
                                                            (push (expand-to v t)))
                                                          (push (set-first (top-grid)))
                                                          (range level))))
      (make-vertices!))))

(make-grid!)

(define mouse-down? false)
(define mouse-down-x 0)
(define mouse-down-y 0)
(define mouse-down-latitude latitude)
(define mouse-down-longitude longitude)
(define milliseconds-between-frames 70.0)
(define last-draw (current-inexact-milliseconds))

(define-values
  (display-width display-height)
  (get-display-size))

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
        
        (glBindBuffer GL_ARRAY_BUFFER vertex-buffer)
        
        (glVertexPointer 3 GL_FLOAT vertex-size 0)
        
        (glColorPointer 4 GL_UNSIGNED_BYTE vertex-size (* 3 float-size))
        
        (glBindBuffer GL_ARRAY_BUFFER 0)
        (glEnableClientState GL_VERTEX_ARRAY)
        (glEnableClientState GL_COLOR_ARRAY)
        
        (glClear GL_COLOR_BUFFER_BIT)
        
        (glBindBuffer GL_ELEMENT_ARRAY_BUFFER index-buffer)
        
        (glDrawElements GL_TRIANGLES
                        (cvector-length indices)
                        GL_UNSIGNED_INT
                        0)
        
        (glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0)
        
        (glDisableClientState GL_VERTEX_ARRAY)
        (glDisableClientState GL_COLOR_ARRAY))
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
         [#\w (begin
                (set-level! (+ level 1))
                (make-grid!)
                (with-gl-context init-gl)
                (repaint!))]
         [#\e (begin
                (set-level! (max (- level 1) base-level))
                (make-grid!)
                (with-gl-context init-gl)
                (repaint!))]
         [_ (void)]))
     (define/override (on-event event)
       (if (send event button-up? 'left)
           (begin
             (set! mouse-down? false)
             (make-grid!)
             (with-gl-context init-gl)
             (repaint!))
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

(send canvas with-gl-context init-gl)
