#lang typed/racket

(provide tile-renderer%)

(require vraid/typed-gl
         vraid/color
         "../planet/grid-base.rkt")

(define tile-renderer%
  (class object%
    (super-new)
    (: buffer-tile-count Integer)
    (define buffer-tile-count 0)
    (: buffer gl-buffer)
    (define buffer (make-gl-buffer 0 0))
    (: resize-buffer? (-> grid Boolean))
    (define (resize-buffer? grid)
      (not (= buffer-tile-count (tile-count grid))))
    (: resize-buffer (-> grid Void))
    (define/public (resize-buffer grid)
      (when (resize-buffer? grid)
        (let* ([tile-count (tile-count grid)]
               [buf (make-gl-buffer (* 7 tile-count) (* 18 tile-count))])
          (set! buffer-tile-count tile-count)
          (set! buffer buf))))
    (: set-shapes (-> grid (-> FlVector (-> FlVector FlVector)) Void))
    (define/public (set-shapes grid transform)
      (init-buffer grid transform buffer)
      ((gl-buffer-bind buffer)))
    (: set-colors (-> grid (-> grid Integer flcolor) Void))
    (define/public (set-colors grid color-function)
      (update-vertices! (tile-count grid)
                        buffer
                        (curry color-function grid))
      ((gl-buffer-bind buffer)))
    (define/public (render)
      (gl-cull-face 'back)
      ((gl-buffer-draw buffer)))))

(: init-buffer (-> grid (-> FlVector (-> FlVector FlVector)) gl-buffer Void))
(define (init-buffer grid transform buffer)
  (let ([set-coord (gl-buffer-set-vertex-coord! buffer)]
        [set-index (gl-buffer-set-index! buffer)])
    (for ([n (tile-count grid)])
      (let ([f (transform ((grid-tile-coordinates grid) n))])
        (let ([k (* n 7)])
          (set-coord k (f ((grid-tile-coordinates grid) n))))
        (for ([i 6])
          (let ([k (+ 1 i (* n 7))])
            (set-coord k (f ((grid-corner-coordinates grid) ((grid-tile-corner grid) n i)))))
          (let ([k (+ (* i 3) (* n 18))])
            (set-index k (* n 7))
            (set-index (+ 1 k) (+ 1 (modulo i 6) (* n 7)))
            (set-index (+ 2 k) (+ 1 (modulo (+ i 1) 6) (* n 7)))))))))

(: update-vertices! (-> Integer gl-buffer (-> Integer flcolor) Void))
(define (update-vertices! count buffer color-function)
  (let* ([set-color (gl-buffer-set-vertex-color! buffer)])
    (for ([n count])
      (let ([color (color-function n)])
        (set-color (* n 7) color)
        (for ([i 6])
          (set-color (+ 1 i (* n 7)) color))))))
