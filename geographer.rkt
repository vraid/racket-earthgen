#lang racket

(require racket/gui/base
         "dynamic-grid.rkt"
         "quaternion.rkt"
         "opengl.rkt"
         math/flonum
         ffi/cvector
         ffi/unsafe)

(define longitude pi)
(define latitude 0.0)
(define (rotation)
   (quaternion*
    (angle-axis->quaternion (fl/ pi 2.0) (flvector 1.0 0.0 0.0))
    (angle-axis->quaternion latitude (flvector -1.0 0.0 0.0))
    (angle-axis->quaternion longitude (flvector 0.0 0.0 -1.0))))
(define (inverse-rotation)
   (quaternion-inverse
    (quaternion*
     (angle-axis->quaternion (fl/ pi 2.0) (flvector 1.0 0.0 0.0))
     (angle-axis->quaternion latitude (flvector -1.0 0.0 0.0))
     (angle-axis->quaternion longitude (flvector 0.0 0.0 -1.0)))))
(define scale 0.9)

(define (subdivision-level-tile-count n)
  (+ 2 (* 10 (expt 3 n))))

(define base-level 6)
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

(define (->vertex coord color)
  (make-gl-vertex
   (flvector-ref coord 0)
   (flvector-ref coord 1)
   (flvector-ref coord 2)
   (bytes-ref color 1)
   (bytes-ref color 2)
   (bytes-ref color 3)
   (bytes-ref color 0)))

(define (make-vertices!)
  (let ([vertices (make-cvector _gl-vertex (* 7 (set-count grid)))]
        [indices (make-cvector _uint (* 18 (set-count grid)))])
    (begin
      (for ([n (set-count grid)]
            [tile (in-set grid)])
        (begin
          (cvector-set! vertices (* n 7) (->vertex (tile-coordinates tile) (tile-color tile)))
          (for ([i 6])
            (cvector-set! vertices (+ 1 i (* n 7)) (->vertex (corner-coordinates (tile-corner tile i)) (tile-color tile)))
            (let ([k (+ (* i 3) (* n 18))])
              (cvector-set! indices k (* n 7))
              (cvector-set! indices (+ 1 k) (+ 1 (modulo i 6) (* n 7)))
              (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) 6) (* n 7)))))))
      (set-gl-vertex-data vertices)
      (set-gl-index-data indices))))

(define (make-grid!)
  (let* ([base-radius (sqrt 3.2)]
         [radius (* base-radius
                    (sqrt (/ (subdivision-level-tile-count base-level)
                             (subdivision-level-tile-count (max base-level
                                                                level)))))]
         [v (quaternion-vector* (inverse-rotation) (flvector 0.0 0.0 1.0))])
    (begin
      (set! grid (if (zero? level)
                     (top-grid)
                     (expand v
                             (min (sqrt 2.0) radius)
                             (foldl (lambda (n t)
                                      (push (expand-to v t)))
                                    (push (set-first (top-grid)))
                                    (range level)))))
      (make-vertices!))))

(define mouse-down? false)
(define mouse-down-x 0)
(define mouse-down-y 0)
(define mouse-down-latitude latitude)
(define mouse-down-longitude longitude)
(define milliseconds-between-frames 20.0)
(define last-draw (current-inexact-milliseconds))

(define-values
  (display-width display-height)
  (get-display-size))

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
     (define/override (on-char event)
       (match (send event get-key-code)
         ['escape (exit)]
         [#\w (begin
                (set-level! (+ level 1))
                (send canvas with-gl-context make-grid!)
                (repaint!))]
         [#\e (begin
                (set-level! (max (- level 1) base-level))
                (send canvas with-gl-context make-grid!)
                (repaint!))]
         [_ (void)]))
     (define/override (on-event event)
       (if (send event button-up? 'left)
           (begin
             (set! mouse-down? false)
             (send canvas with-gl-context make-grid!)
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
               (when (send event button-down? 'left)
                 (begin
                   (set! mouse-down? true)
                   (set! mouse-down-x (send event get-x))
                   (set! mouse-down-y (send event get-y))
                   (set! mouse-down-latitude latitude)
                   (set! mouse-down-longitude longitude))))))
     (super-instantiate () (style '(gl))))
   [parent frame]))

(define gl-context canvas)

(send frame maximize #t)
(send frame show #t)
(send canvas focus)
(send canvas with-gl-context make-grid!)
(send canvas on-paint)
