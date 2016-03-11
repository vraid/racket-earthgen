#lang typed/racket

(provide canvas-button%)

(require typed/racket/gui/base
         vraid/flow)

(define color-white (make-object color% 255 255 255))
(define color-grey (make-object color% 240 240 240))

(define canvas-button%
  (class canvas%
    (super-new)
    (init-field [label : String]
                [default-color : (Instance Color%) color-grey]
                [hover-color : (Instance Color%) color-white]
                [on-click : (Option (-> Any)) #f])
    (inherit enable
             show
             is-enabled?
             refresh-now
             get-dc
             set-canvas-background)
    (set-canvas-background default-color)
    (define/override (on-event event)
      (case (send event get-event-type)
        [(leave) (set-canvas-background default-color)]
        [(left-down) (and-call on-click)]
        [(enter left-up) (set-canvas-background hover-color)]
        [else (void)])
      (refresh-now))
    (define/override (on-paint)
      (let ([dc (get-dc)]
            [text (if (is-enabled?)
                      label
                      "")])
        (let-values ([(text-width text-height _x _y) (send dc get-text-extent text)]
                     [(dc-width dc-height) (send dc get-size)])
          (let ([x (/ (- dc-width text-width) 2)]
                [y (/ (- dc-height text-height) 2)])
            (send dc draw-text text x y)))))))
