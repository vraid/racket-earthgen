#lang typed/racket

(provide mouse-input-handler%)

(require typed/racket/gui)

(define-type point (Vector Integer Integer))

(define mouse-input-handler%
  (class object%
    (super-new)
    (field [left-down? : Boolean #f]
           [dragging? : Boolean #f]
           [last-position : point (vector 0 0)])
    [init-field [on-down : (point -> Any)]
                [on-click : (point -> Any)]
                [on-drag : (point point -> Any)]]
    (: mouse-event ((Instance Mouse-Event%) -> Void))
    (define/public (mouse-event event)
      (let* ([x (send event get-x)]
             [y (send event get-y)]
             [position (vector x y)])
        (begin
          (cond
            [(send event button-changed? 'left)
             (when (send event button-down? 'left)
               (on-down position))
             (when (and left-down? (send event button-up? 'left) (not dragging?))
               (on-click position))
             (set! left-down? (send event button-down? 'left))
             (set! dragging? #f)]
            [(send event get-left-down)
             (when dragging?
               (on-drag last-position position))]
            [else #f])
          (set! dragging? (and left-down?
                               (or dragging?
                                   (send event moving?))))
          (set! last-position position))))))
