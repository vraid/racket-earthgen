#lang racket

(provide generation-parameters-panel%)

(require racket/gui/base
         vraid/flow
         "edit-panel.rkt"
         "data-format.rkt"
         "../planet/planet.rkt")

(define generation-parameters-panel%
  (class vertical-panel%
    (super-new)
    (init-field current-planet
                [control-height 30]
                [label-width 120])
    (define (read-only-edit parent caption converter get-value)
      ((read-only-panel parent control-height label-width) caption (convert-to-string converter) get-value))
    (define grid-size-edit (read-only-edit this "grid size" format-integer (thunk (grid-subdivision-level (current-planet)))))
    (define sea-level-edit (read-only-edit this "sea level" format-positional (thunk (and-let* ([planet (current-planet)]
                                                                                                [_ (planet-water? planet)])
                                                                                               (planet-water-sea-level (current-planet))))))
    (define controls (list grid-size-edit
                           sea-level-edit))
    (define/public (update/planet planet)
      (for ([c controls])
        (send c update)))))
