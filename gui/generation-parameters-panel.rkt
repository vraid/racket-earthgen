#lang racket

(provide (all-defined-out))

(require racket/gui/base
         vraid/flow
         vraid/struct
         "edit-panel.rkt"
         "data-format.rkt"
         "../planet/grid-base.rkt"
         "../planet/geometry-base.rkt"
         "../planet/terrain-base.rkt"
         "../planet/water-base.rkt"
         "../planet/climate-base.rkt")

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
    (define (current-planet/condition cond?)
      (let ([planet (current-planet)])
        (and (cond? planet) planet)))
    (define (current-terrain)
      (current-planet/condition planet-water?))
    (define (current-climate)
      (current-planet/condition planet-climate?))
    (define climate-param (default-climate-parameters))
    (define (edit parent caption converter get-value)
      ((edit-panel parent control-height label-width) caption (convert-to-string converter) (convert-from-string converter) get-value #f))
    (define (read-only parent caption converter get-value)
      ((read-only-panel parent control-height label-width) caption (convert-to-string converter) get-value))
    (define terrain-panel (new-panel))
    (define grid-size-edit (edit terrain-panel
                                 "grid size"
                                 format-integer
                                 (thunk (grid-subdivision-level (current-planet)))))
    (define radius-edit (edit terrain-panel
                              "radius"
                              format-positional
                              (thunk (and-let* ([planet (current-terrain)])
                                       (planet-radius planet)))))
    (define sea-level-edit (edit terrain-panel
                                 "sea level"
                                 format-positional
                                 (thunk (and-let* ([planet (current-terrain)])
                                          (planet-water-sea-level planet)))))
    (define axis-edit (read-only terrain-panel
                                 "axis"
                                 format-flvector
                                 (thunk (planet-geometry-axis (current-planet)))))
    (define selected-axis-edit (read-only terrain-panel
                                          "selected axis"
                                          format-flvector
                                          (thunk selected-axis)))
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
    (define delta-edit (edit climate-panel
                             "acceptable delta"
                             format-positional
                             (thunk (climate-parameters-acceptable-delta climate-param))))
    (define precipitation-factor-edit (edit climate-panel
                                            "precipitation factor"
                                            format-positional
                                            (thunk (climate-parameters-precipitation-factor climate-param))))
    (define humidity-half-life-edit (edit climate-panel
                                          "humidity half life"
                                          format-positional
                                          (thunk (climate-parameters-humidity-half-life-days climate-param))))
    (define generate-climate-button (new button%
                                         [parent climate-panel]
                                         [label "generate climate"]
                                         [callback generate-climate]))
    (define controls (list grid-size-edit
                           radius-edit
                           sea-level-edit
                           axis-edit
                           selected-axis-edit
                           delta-edit
                           precipitation-factor-edit
                           humidity-half-life-edit))
    (define/public (select-axis v)
      (set! selected-axis v)
      (send selected-axis-edit update)
      (send change-axis-button enable v))
    (define/public (set-axis)
      (when selected-axis
        (send selected-axis-edit update)
        (send axis-edit update/value selected-axis)))
    (define/public (terrain-parameters)
      (terrain-parameters/kw
       #:grid-size (let ([size (send grid-size-edit get-value)])
                     (if (and (integer? size) (<= 0 size))
                         (inexact->exact size)
                         0))
       #:radius (let ([radius (send radius-edit get-value)])
                  (if (and (real? radius) (positive? radius))
                      (exact->inexact radius)
                      default-radius))
       #:sea-level (let ([sea-level (send sea-level-edit get-value)])
                     (if (real? sea-level) (exact->inexact sea-level) 0.0))
       #:axis (or (send axis-edit get-value) default-axis)))
    (define/public (climate-parameters)
      (set! climate-param
            (climate-parameters/kw
             #:axial-tilt 0.0
             #:acceptable-delta (let ([delta (send delta-edit get-value)])
                                  (if (and (real? delta) (< 0 delta))
                                      delta
                                      0.05))
             #:precipitation-factor (let ([factor (send precipitation-factor-edit get-value)])
                                      (if (and (real? factor) (<= 0 factor))
                                          factor
                                          1.0))
             #:humidity-half-life-days (let ([days (send humidity-half-life-edit get-value)])
                                         (if (and (real? days) (< 0 days))
                                             days
                                             5.0))
             #:seasons-per-cycle 16))
      climate-param)
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
