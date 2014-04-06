#lang racket

(require racket/gui/base
         "quaternion.rkt"
         "planet.rkt"
         "planet-create.rkt"
         "climate-create.rkt"
         "grid.rkt"
         "color.rkt"
         "planet-color.rkt"
         "opengl.rkt"
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

(define mouse-down? false)
(define mouse-down-x 0)
(define mouse-down-y 0)
(define mouse-down-latitude latitude)
(define mouse-down-longitude longitude)
(define milliseconds-between-frames 70.0)
(define last-draw (current-inexact-milliseconds))

(define planet-box (box #f))
(define grid-box (box (n-grid-list null 0)))
(define color-mode color-topography)

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

(define (make-vertices! color-mode)
  (let* ([p (unbox planet-box)]
         [vertices (make-cvector _gl-vertex (* 7 (tile-count p)))]
         [indices (make-cvector _uint (* 18 (tile-count p)))])
    (begin
      (for ([n (tile-count p)])
        (let ([color (color-mode n)])
          (begin
            (cvector-set! vertices (* n 7) (->gl-vertex (tile-coordinates p n) color))
            (for ([i 6])
              (begin
                (cvector-set! vertices (+ 1 i (* n 7)) (->gl-vertex (corner-coordinates p (tile-corner p n i)) color))
                (let ([k (+ (* i 3) (* n 18))])
                  (cvector-set! indices k (* n 7))
                  (cvector-set! indices (+ 1 k) (+ 1 (modulo i 6) (* n 7)))
                  (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) 6) (* n 7)))))))))
      (set-gl-vertex-data vertices)
      (set-gl-index-data indices))))

(define frame
  (new frame%
       [label "earthgen"]
       [width window-width]
       [height window-height]))

(define (color-vector planet f)
  (let ([p (unbox planet-box)])
    (build-vector
     (vector-length (grid-tile-count (planet-grid p)))
     (lambda (n)
       (f p n)))))

(define canvas
  (new
   (class* canvas% ()
     (inherit with-gl-context swap-gl-buffers)
     (define/override (on-paint)
       (when (fl< milliseconds-between-frames
                  (fl- (current-inexact-milliseconds) last-draw))
         (thread
          (thunk
            (begin
              (with-gl-context
               (thunk
                 (begin
                   (let ([mx (fl* (fl/ 1.0 scale) (exact->inexact (/ display-width display-height)))]
                         [my (fl/ 1.0 scale)])
                     (set-gl-ortho-projection (- mx) mx (- my) my -2.0 2.0))
                   (for ([quat (list (list 90.0 -1.0 0.0 0.0)
                                     (list (fl* (fl/ 180.0 pi) latitude) 1.0 0.0 0.0)
                                     (list (fl* (fl/ 180.0 pi) longitude) 0.0 0.0 1.0))])
                     (rotate-gl quat))
                   (draw-gl)
                   (swap-gl-buffers))))
              (set! last-draw (current-inexact-milliseconds)))))))
     (define/override (on-size width height)
       (begin
         (set! display-width width)
         (set! display-height height)
         (with-gl-context
          (thunk
            (set-gl-viewport 0 0 width height)))))
     (define (repaint!)
       (begin
         (set! last-draw 0.0)
         (on-paint)))
     (define (color-planet! f)
       (when (planet? (unbox planet-box))
         (thread
          (thunk
            (begin
              (set! color-mode f)
              (with-gl-context
               (thunk
                 (make-vertices! (curry color-mode (unbox planet-box)))))
              (repaint!))))))
     (define (generate-terrain!)
       (thread
        (thunk
          (let-values ([(size method) (load "terrain-gen.rkt")])
            (let ([grids (n-grid-list (unbox grid-box) size)])
              (begin
                (set-box! grid-box grids)
                (set-box! planet-box ((heightmap->planet (first grids)) (method grids)))
                (color-planet! color-mode)))))))
     (define/override (on-char event)
       (define key-code (send event get-key-code))
       (match key-code
         ['escape (exit)]
         [#\q (generate-terrain!)]
         [#\w (let ([p (unbox planet-box)])
                (when (planet? p)
                  (begin
                    (set-box! planet-box (climate-next (default-climate-parameters) p))
                    (color-planet! color-mode))))]
         [#\a (color-planet! base-color)]
         [#\s (color-planet! color-topography)]
         [#\d (color-planet! color-temperature)]
         [#\f (color-planet! color-albedo)]
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
                 (repaint!))
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

;(send frame maximize #t)
(send frame show #t)
(send canvas focus)
(send canvas with-gl-context (thunk (set-gl-vertex-data (make-cvector _gl-vertex 0))))
(send canvas with-gl-context (thunk (set-gl-index-data (make-cvector _uint 0))))
