#lang racket

(provide undo-button)

(require racket/gui/base)

(define color-white (make-object color% 255 255 255))
(define color-grey (make-object color% 240 240 240))

(define canvas-button%
  (class canvas%
    (super-new)
    (init-field label
                default-color
                hover-color
                on-click)
    (inherit enable
             show
             is-enabled?
             refresh-now
             get-dc
             set-canvas-background)
    (define/public (enable-button enable?)
      (enable enable?)
      (show enable?)
      (unless enable? (set-canvas-background default-color))
      (refresh-now))
    (define/override (on-event event)
      (case (send event get-event-type)
        [(leave) (set-canvas-background default-color)]
        [(left-down) (on-click)]
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

(define ((canvas-button parent) width height label on-click)
  (let* ([default-color color-grey]
         [canvas (new canvas-button%
                      [label label]
                      [on-click on-click]
                      [default-color default-color]
                      [hover-color color-white]
                      [parent parent]
                      [min-width (if width width 0)]
                      [min-height (if height height 0)]
                      [stretchable-width (not width)]
                      [stretchable-height (not height)])])
    (send canvas set-canvas-background default-color)
    canvas))

(define ((undo-button parent) width height on-click)
  (let ([button ((canvas-button parent) width height "↺" on-click)])
    (send button enable-button #f)
    button))
