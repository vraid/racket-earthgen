#lang racket

(provide edit-panel
         read-only-panel)

(require racket/gui/base
         "canvas-button.rkt"
         "edit-field.rkt")

(define ((read-only-panel parent height label-width) label value->string get-value)
  (base-panel parent height label-width label #t value->string (lambda (s) #f) get-value #f))

(define ((edit-panel parent height label-width) label value->string string->value get-value on-enter)
  (base-panel parent height label-width label #f value->string string->value get-value on-enter))

(define (base-panel parent height label-width label read-only? value->string string->value get-value on-enter)
  (letrec ([panel (new (class horizontal-panel%
                         (super-new)
                         (define/public (get-value)
                           (send edit current-value))
                         (define/public (update/value v)
                           (send edit update/value v))
                         (define/public (update)
                           (send edit update)))
                       [parent parent]
                       [min-height height]
                       [stretchable-height #f])]
           [message (new message%
                         [parent panel]
                         [label label]
                         [min-width label-width]
                         [stretchable-width #f])]
           [filler (new horizontal-panel%
                        [parent panel])]
           [edit (new edit-field%
                      [read-only? read-only?]
                      [label #f]
                      [stretchable-width #t]
                      [stretchable-height #f]
                      [parent filler]
                      [value get-value]
                      [value->string value->string]
                      [string->value string->value]
                      [on-enter on-enter]
                      [on-change (lambda (v)
                                   (send undo show (not (equal? v (get-value)))))])]
           [undo (let ([height (send edit get-height)])
                   (new canvas-button%
                        [label "↺"]
                        [parent panel]
                        [min-width height]
                        [min-height height]
                        [stretchable-width #f]
                        [stretchable-height #f]
                        [on-click (thunk (send edit update))]))])
    panel))
