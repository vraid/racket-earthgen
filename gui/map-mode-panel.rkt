#lang racket

(provide (all-defined-out))

(require racket/gui/base
         vraid/flow
         "custom-choice.rkt"
         "../map-mode.rkt"
         "../projections.rkt")

(define map-mode-panel%
  (class vertical-panel%
    (super-new)
    (init-field projections
                on-projection-select
                color-modes
                on-color-select)
    (define projection-choices (list->vector projections))
    (define projection-choice
      (new custom-choice%
           [label "projection"]
           [stretchable-width #t]
           [choices (map symbol->string projections)]
           [parent this]
           [callback (λ (c e)
                       (let ([choice (vector-ref projection-choices (send c get-selection))])
                         (on-projection-select choice)))]))
    (define/public (select-projection proj)
      (when-let ([index (vector-member proj projection-choices)])
                (send projection-choice set-selection index)))
    (define current-color-mode-choices (vector))
    (define color-mode-choice
      (new custom-choice%
           [label "map mode"]
           [stretchable-width #t]
           [choices '()]
           [parent this]
           [callback (λ (c e)
                       (let ([current-choice (vector-ref current-color-mode-choices (send c get-selection))])
                         (on-color-select current-choice)))]))
    (define/public (select-color-mode mode)
      (when-let ([index (vector-member mode current-color-mode-choices)])
                (send color-mode-choice set-selection index)))
    (define/public (enable-color-modes planet)
      (set! current-color-mode-choices (vector-filter (lambda (m)
                                                        ((map-mode-condition m) planet))
                                                      color-modes))
      (send color-mode-choice set-choices (map (lambda (m)
                                               (symbol->string (map-mode-name m)))
                                             (vector->list current-color-mode-choices))))))
