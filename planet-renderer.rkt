#lang racket

(provide planet-renderer%)

(require "opengl.rkt"
         "color.rkt"
         "planet.rkt"
         racket/flonum
         ffi/cvector
         ffi/unsafe)

(define planet-renderer%
  (class object%
    (super-new)
    (init-field planet)
    (define color-function (thunk* (flcolor3 0.0 0.0 0.0)))
    (define buffer-tile-count 0)
    (define tile-vertex-buffer
      (gl-vertex-buffer
       (generate-gl-buffer-handle)
       (make-cvector _uint 0)))
    (define tile-index-buffer
      (gl-index-buffer
       (generate-gl-buffer-handle)
       (make-cvector _uint 0)))
    (define (update-buffer-size)
      (let ([planet (planet)])
        (unless (or (not planet) (= buffer-tile-count (grid-tile-count planet)))
          (let ([b (make-tile-buffer planet)])
            (set! buffer-tile-count (tile-buffer-tile-count b))
            (set! tile-index-buffer
                  (gl-index-buffer
                   (gl-index-buffer-handle tile-index-buffer)
                   (tile-buffer-indices b)))
            (set! tile-vertex-buffer
                  (gl-vertex-buffer
                   (gl-vertex-buffer-handle tile-vertex-buffer)
                   (tile-buffer-vertices b)))
            (set-gl-vertex-buffer! tile-vertex-buffer)
            (set-gl-index-buffer! tile-index-buffer)))))
    (define/public (set-tile-colors f)
      (set! color-function f)
      (update-buffer-size)
      (update-vertices! (grid-tile-count (planet))
                        tile-vertex-buffer
                        (curry f (planet)))
      (set-gl-vertex-buffer! tile-vertex-buffer))
    (define/public (render)
      (update-buffer-size)
      (gl-clear (list 0.0 0.0 0.0 0.0))
      (gl-cull-face 'back)
      (gl-draw tile-vertex-buffer
               tile-index-buffer))))

(struct tile-buffer
  (tile-count vertices indices))

(define (->gl-vertex coord color)
  (make-gl-vertex
   (flvector-ref coord 0)
   (flvector-ref coord 1)
   (flvector-ref coord 2)
   (flcolor->byte (flcolor-red color))
   (flcolor->byte (flcolor-green color))
   (flcolor->byte (flcolor-blue color))
   (flcolor->byte (flcolor-alpha color))))

(define (make-tile-buffer grid)
  (let* ([tile-count (grid-tile-count grid)]
         [vertices (make-cvector _gl-vertex (* 7 tile-count))]
         [indices (make-cvector _uint (* 18 tile-count))]
         [color (flcolor3 0.0 0.0 0.0)])
    (for ([n tile-count])
      (cvector-set! vertices (* n 7) (->gl-vertex ((grid-tile-coordinates grid) n) color))
      (for ([i 6])
        (cvector-set! vertices (+ 1 i (* n 7)) (->gl-vertex ((grid-corner-coordinates grid) ((grid-tile-corner grid) n i))
                                                            color))
        (let ([k (+ (* i 3) (* n 18))])
          (cvector-set! indices k (* n 7))
          (cvector-set! indices (+ 1 k) (+ 1 (modulo i 6) (* n 7)))
          (cvector-set! indices (+ 2 k) (+ 1 (modulo (+ i 1) 6) (* n 7))))))
    (tile-buffer
     tile-count
     vertices
     indices)))

(define (set-vertex-color! vertices n color)
  (let ([v (cvector-ref vertices n)])
    (set-gl-vertex-red! v (byte-color-red color))
    (set-gl-vertex-green! v (byte-color-green color))
    (set-gl-vertex-blue! v (byte-color-blue color))))

(define (update-vertices! count buffer color-function)
  (let* ([vertices (gl-vertex-buffer-data buffer)])
    (for ([n count])
      (let ([color (flcolor->byte-color (color-function n))])
        (set-vertex-color! vertices (* n 7) color)
        (for ([i 6])
          (set-vertex-color! vertices (+ 1 i (* n 7)) color))))))
