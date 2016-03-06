#lang typed/racket

(provide tile-renderer%)

(require vraid/typed-gl
         vraid/color
         "../planet/planet.rkt")

(define tile-renderer%
  (class object%
    (super-new)
    (init-field [planet : (-> grid)])
    (: buffer-tile-count Integer)
    (define buffer-tile-count 0)
    (: buffer gl-buffer)
    (define buffer (make-gl-buffer 0 0))
    (: resize-buffer? (-> Boolean))
    (define (resize-buffer?)
      (not (= buffer-tile-count (tile-count (planet)))))
    (: resize-buffer (-> Void))
    (define/public (resize-buffer)
      (when (resize-buffer?)
        (let* ([tile-count (tile-count (planet))]
               [buf (make-gl-buffer (* 7 tile-count) (* 18 tile-count))])
          (set! buffer-tile-count tile-count)
          (set! buffer buf)
          (init-buffer (planet) buffer))))
    (: remake-buffer ((grid Integer -> flcolor) -> Void))
    (define/public (remake-buffer color-function)
      (update-vertices! (tile-count (planet))
                        buffer
                        (curry color-function (planet)))
      ((gl-buffer-bind buffer)))
    (define/public (render)
      (gl-cull-face 'back)
      ((gl-buffer-draw buffer)))))

(: init-buffer (grid gl-buffer -> Void))
(define (init-buffer grid buffer)
  (let ([set-coord (gl-buffer-set-vertex-coord! buffer)]
        [set-color (gl-buffer-set-vertex-color! buffer)]
        [set-index (gl-buffer-set-index! buffer)]
        [color (flcolor3 0.0 0.0 0.0)])
    (for ([n (tile-count grid)])
      (let ([k (* n 7)])
        (set-coord k ((grid-tile-coordinates grid) n))
        (set-color k color))
      (for ([i 6])
        (let ([k (+ 1 i (* n 7))])
          (set-coord k ((grid-corner-coordinates grid) ((grid-tile-corner grid) n i)))
          (set-color k color))
        (let ([k (+ (* i 3) (* n 18))])
          (set-index k (* n 7))
          (set-index (+ 1 k) (+ 1 (modulo i 6) (* n 7)))
          (set-index (+ 2 k) (+ 1 (modulo (+ i 1) 6) (* n 7))))))))

(: update-vertices! (Integer gl-buffer (Integer -> flcolor) -> Void))
(define (update-vertices! count buffer color-function)
  (let* ([set-color (gl-buffer-set-vertex-color! buffer)])
    (for ([n count])
      (let ([color (color-function n)])
        (set-color (* n 7) color)
        (for ([i 6])
          (set-color (+ 1 i (* n 7)) color))))))
