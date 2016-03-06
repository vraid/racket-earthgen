#lang racket

(provide tile-data-panel%)

(require vraid/flow
         vraid/math
         racket/gui/base
         "data-format.rkt"
         "edit-panel.rkt"
         "../planet/planet.rkt")

(define no-frame
  (new frame%
       [label ""]))

(define tile-data-panel%
  (class vertical-panel%
    (super-new)
    (init-field current-planet
                [control-height 30]
                [label-width 120])
    (define tile #f)
    (define ((planet-type type?))
      (let ([planet (current-planet)])
        (and (type? planet) planet)))
    (define terrain (planet-type planet-terrain?))
    (define climate (planet-type planet-climate?))
    (define (update/maybe-tile tile)
      (when (or (not tile) ((index-within-range? 0 (tile-count (current-planet))) tile))
        (update/tile tile)))
    (define (new-panel)
      (new vertical-panel%
           [parent this]))
    (define general-panel (new-panel))
    (define terrain-panel (new-panel))
    (define climate-panel (new-panel))
    (define (new-edit parent)
      (edit-panel parent control-height label-width))
    (define (read-only-edit parent caption converter get-value)
      ((read-only-panel parent control-height label-width) caption (convert-to-string converter) get-value))
    (define ((type-edit parent get-planet) caption converter function)
      (read-only-edit parent
                      caption
                      converter
                      (thunk (and-let ([planet (get-planet)])
                                      (and tile (function planet tile))))))
    (define terrain-edit (type-edit terrain-panel terrain))
    (define climate-edit (type-edit climate-panel climate))
    (define id-edit ((new-edit general-panel) "tile id" integer->string string->integer (thunk tile) update/maybe-tile))
    (define elevation-edit (terrain-edit "elevation" format-positional tile-elevation))
    (define temperature-edit (climate-edit "temperature" format-positional tile-temperature))
    (define insolation-edit (climate-edit "insolation" format-exponential tile-insolation))
    (define aridity-edit (climate-edit "aridity" format-positional tile-aridity))
    (define relative-humidity-edit (climate-edit "relative humidity" format-positional tile-relative-humidity))
    (define absolute-humidity-edit (climate-edit "absolute humidity" format-exponential tile-humidity))
    (define precipitation-edit (climate-edit "precipitation" format-exponential tile-precipitation))
    (define controls (list id-edit
                           elevation-edit
                           temperature-edit
                           insolation-edit
                           aridity-edit
                           relative-humidity-edit
                           absolute-humidity-edit
                           precipitation-edit))
    (define/public (update/tile n)
      (set! tile n)
      (for ([c controls])
        (send c update)))
    (define/public (update/planet planet)
      (send terrain-panel show (planet-terrain? planet))
      (send climate-panel show (planet-climate? planet))
      (update/tile tile))))
