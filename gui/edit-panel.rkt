#lang racket

(provide edit-panel)

(require racket/gui/base)

(define color-white (make-object color% 255 255 255))
(define color-grey (make-object color% 240 240 240))

(define ((canvas-button parent) width height label on-click)
  (define default-color color-grey)
  (define hover-color color-white)
  (letrec ([canvas
         (new (class* canvas% ()
                (super-instantiate ())
                
                (define (enable-button enable?)
                  (send canvas enable enable?)
                  (unless enable? (send canvas set-canvas-background default-color))
                  (send canvas refresh-now))
                (public enable-button)
                
                (define/override (on-event event)
                  (case (send event get-event-type)
                    [(leave) (send canvas set-canvas-background default-color)]
                    [(left-down) (on-click)]
                    [(enter left-up) (send canvas set-canvas-background hover-color)]
                    [else (void)])
                  (send canvas refresh-now))
                
                (define/override (on-paint)
                  (let ([dc (send canvas get-dc)]
                        [text (if (send canvas is-enabled?)
                                  label
                                  "")])
                    (let-values ([(text-width text-height _x _y) (send dc get-text-extent text)]
                                 [(dc-width dc-height) (send dc get-size)])
                      (let ([x (/ (- dc-width text-width) 2)]
                            [y (/ (- dc-height text-height) 2)])
                        (send dc draw-text text x y))))))
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

(define ((edit-panel parent height label-width) label get set)
  (let* ([panel (new horizontal-panel%
                     [parent parent]
                     [min-height height]
                     [stretchable-height #f])]
         [message (new message%
                       [parent panel]
                       [label label]
                       [min-width label-width]
                       [stretchable-width #f])]
         [filler (new horizontal-panel%
                      [parent panel])])
    (letrec ([edit (new text-field%
                        [label #f]
                        [parent filler]
                        [stretchable-width #t]
                        [callback (lambda (edit event)
                                    (begin
                                      (when (equal? 'text-field-enter (send event get-event-type))
                                        (set (send edit get-value)))
                                      (on-change)))]
                        [init-value (get)])]
             [undo ((undo-button panel) (send edit get-height) (send edit get-height) (thunk (update-edit)))]
             [on-change (thunk (send undo enable-button (not (equal? (get)
                                                                     (send edit get-value)))))]
             [update-edit (thunk (send edit set-value (get))
                                 (on-change))])
      panel)))
