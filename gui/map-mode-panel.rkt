#lang racket

(require racket/gui/base
         vraid/flow
         "custom-choice.rkt")

(provide (all-defined-out))

(define (map-mode-panel% map-mode-name)
  (class vertical-panel%
    (super-new)
    (init-field color-modes
                on-color-select)
    (define vec (list->vector color-modes))
    (define color-mode-choice
      (new custom-choice%
           [label "map mode"]
           [stretchable-width #t]
           [choices (map (compose symbol->string map-mode-name)
                         color-modes)]
           [parent this]
           [callback (λ (c e)
                       (let ([current-choice (vector-ref vec (send c get-selection))])
                         (on-color-select current-choice)))]))
    (define/public (select-color-mode mode)
      (when-let ([index (vector-member mode vec)])
                (send color-mode-choice set-selection index)))))
