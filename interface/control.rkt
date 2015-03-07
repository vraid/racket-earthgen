#lang typed/racket

(provide (all-defined-out))

(require typed/racket/class
         typed/racket/gui
         vraid/math
         vraid/types
         vraid/flow
         math/flonum
         "../planet/planet.rkt")

(struct: point
  ([x : Integer]
   [y : Integer])
  #:transparent)

(define origin
  (point 0 0))

(: point-equal? (point point -> Boolean))
(define (point-equal? p q)
  (and (equal? (point-x p) (point-x q))
       (equal? (point-y p) (point-y q))))

(: point-subtract (point point -> point))
(define (point-subtract p q)
  (point (- (point-x q) (point-x p))
         (- (point-y q) (point-y p))))

(define planet-control%
  (class object%
    (super-new)
    (field [mouse-down? : Boolean #f]
           [mouse-moving? : Boolean #f]
           [wheel-delta : Real 0.0]
           [mouse-down-position : point origin]
           [mouse-position : point origin]
           [last-mouse-position : point origin])
    (init-field [viewport-width : Integer]
                [viewport-height : Integer]
                [scale : Flonum]
                [scale-max : (maybe Flonum) #f]
                [scale-min : (maybe Flonum) #f])
    (: on-event ((Instance Mouse-Event%) -> Any))
    (define/public (on-event event)
      #f)
    (: resize-viewport (Integer Integer -> Void))
    (define/public (resize-viewport width height)
      (set! scale (* scale (fl (/ viewport-width width))))
      (set! viewport-width width)
      (set! viewport-height height))
    (: scale-by (Flonum -> Void))
    (define/public (scale-by n)
      (set! scale (fl ((within-interval scale-min scale-max) (* scale n)))))
    (: rotation-list (planet -> (Listof Real)))
    (define/public (rotation-list planet)
      (list))
    (: set-projection (-> Any))
    (define/public (set-projection)
      #f)
    (: mouse-up (-> Any))
    (define (mouse-up)
      (set! mouse-down? #f)
      (set! mouse-moving? #f))
    (: update-input ((Instance Mouse-Event%) -> Void))
    (define/public (update-input event)
      (when (send event button-up? 'left)
        (mouse-up))
      (set! last-mouse-position mouse-position)
      (set! mouse-position (point (send event get-x) (send event get-y)))
      (when mouse-down?
        (when (not (point-equal? last-mouse-position
                                 mouse-position))
          (set! mouse-moving? #t)))
      (when (send event button-down? 'left)
        (begin
          (set! mouse-down-position mouse-position)
          (set! mouse-down? #t))))))
