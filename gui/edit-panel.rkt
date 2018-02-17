#lang racket

(require racket/gui/base
         "edit-field.rkt")

(provide read-only-panel)

(define ((read-only-panel parent height label-width) label value->string get-value)
  (base-panel parent height label-width label value->string get-value))

(define (base-panel parent height label-width label value->string get-value)
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
                      [label #f]
                      [stretchable-width #t]
                      [stretchable-height #f]
                      [parent filler]
                      [value get-value]
                      [value->string value->string])])
    panel))
