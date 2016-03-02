#lang racket

(provide (all-defined-out))

(require racket/gui/base
         vraid/flow
         "custom-choice.rkt"
         "../map-mode.rkt")

(define map-mode-panel%
  (class vertical-panel%
    (super-new)
    (init-field modes
                on-select)
    (define current-choices (vector))
    (define map-mode-choice
      (new custom-choice%
           [label "map mode"]
           [stretchable-width #t]
           [choices '()]
           [parent this]
           [callback (lambda (c e)
                       (let ([current-choice (vector-ref current-choices (send c get-selection))])
                         (on-select current-choice)))]))
    (define/public (select-mode mode)
      (when-let ([index (vector-member mode current-choices)])
                (send map-mode-choice set-selection index)))
    (define/public (enable-modes planet)
      (set! current-choices (vector-filter (lambda (m)
                                             ((map-mode-condition m) planet))
                                           modes))
      (send map-mode-choice set-choices (map (lambda (m)
                                               (symbol->string (map-mode-name m)))
                                             (vector->list current-choices))))))
