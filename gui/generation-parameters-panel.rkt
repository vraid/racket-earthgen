#lang racket

(provide (all-defined-out))

(require racket/gui/base
         vraid/flow
         "edit-panel.rkt"
         "data-format.rkt"
         "../planet/planet.rkt")

(struct terrain-generation-parameters
  (grid-size sea-level axis))

(define generation-parameters-panel%
  (class vertical-panel%
    (super-new)
    (init-field current-planet
                generate-terrain
                generate-climate
                [control-height 30]
                [label-width 120])
    (define selected-axis #f)
    (define (new-panel)
      (new vertical-panel%
           [parent this]))
    (define (edit parent caption converter get-value)
      ((edit-panel parent control-height label-width) caption (convert-to-string converter) (convert-from-string converter) get-value #f))
    (define (read-only parent caption converter get-value)
      ((read-only-panel parent control-height label-width) caption (convert-to-string converter) get-value))
    (define terrain-panel (new-panel))
    (define grid-size-edit (edit terrain-panel "grid size" format-integer (thunk (grid-subdivision-level (current-planet)))))
    (define sea-level-edit (edit terrain-panel "sea level" format-positional (thunk (and-let* ([planet (current-planet)]
                                                                                               [_ (planet-water? planet)])
                                                                                              (planet-water-sea-level (current-planet))))))
    (define axis-edit (read-only terrain-panel "axis" format-flvector (thunk (planet-geometry-axis (current-planet)))))
    (define selected-axis-edit (read-only terrain-panel "selected axis" format-flvector (thunk selected-axis)))
    (define change-axis-button (new button%
                                    [parent terrain-panel]
                                    [label "change axis"]
                                    [callback (lambda (b e)
                                                (set-axis))]))
    (define generate-terrain-button (new button%
                                         [parent terrain-panel]
                                         [label "generate terrain"]
                                         [callback generate-terrain]))
    (define climate-panel (new-panel))
    (define generate-climate-button (new button%
                                         [parent climate-panel]
                                         [label "generate climate"]
                                         [callback generate-climate]))
    (define controls (list grid-size-edit
                           sea-level-edit
                           axis-edit
                           selected-axis-edit))
    (define/public (select-axis v)
      (set! selected-axis v)
      (send selected-axis-edit update)
      (send change-axis-button enable v))
    (define/public (set-axis)
      (when selected-axis
        (send selected-axis-edit update)
        (send axis-edit update/value selected-axis)))
    (define/public (terrain-parameters)
      (terrain-generation-parameters
       (let ([size (send grid-size-edit get-value)])
         (if (and (integer? size) (<= 0 size))
             (inexact->exact size)
             0))
       (let ([sea-level (send sea-level-edit get-value)])
         (if (real? sea-level) (exact->inexact sea-level) 0.0))
       (or (send axis-edit get-value) default-axis)))
    (define/public (enable-buttons a)
      (for ([b (list generate-terrain-button
                     generate-climate-button)])
        (send b enable a)))
    (define (update-controls controls)
      (for ([c controls])
        (send c update)))
    (define/public (update/planet planet)
      (select-axis #f)
      (send climate-panel show (planet-water? planet))
      (update-controls controls))))
