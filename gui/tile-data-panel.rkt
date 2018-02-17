#lang racket

(require vraid/math
         racket/gui/base
         "data-format.rkt"
         "edit-panel.rkt"
         "../planet/planet.rkt")

(provide tile-data-panel%)

(define no-frame
  (new frame%
       [label ""]))

(define tile-data-panel%
  (class vertical-panel%
    (super-new)
    (init-field planet
                [control-height 30]
                [label-width 120])
    (define tile #f)
    (define ((planet-type type?))
      (and (type? planet) planet))
    (define terrain (planet-type planet-terrain?))
    (define climate (planet-type planet-climate?))
    (define (update/maybe-tile tile)
      (when (or (not tile) ((index-within-range? 0 (tile-count planet)) tile))
        (update/tile tile)))
    (define (new-panel)
      (new vertical-panel%
           [parent this]))
    (define general-panel (new-panel))
    (define terrain-panel (new-panel))
    (define climate-panel (new-panel))
    (define (read-only-edit parent caption converter get-value)
      ((read-only-panel parent control-height label-width) caption converter get-value))
    (define ((type-edit parent get-planet) caption converter function)
      (read-only-edit parent
                      caption
                      converter
                      (thunk (and tile (function planet tile)))))
    (define terrain-edit (type-edit terrain-panel terrain))
    (define climate-edit (type-edit climate-panel climate))
    (define id-edit (read-only-edit general-panel "tile id" integer->string (thunk tile)))
    (define elevation-edit (terrain-edit "elevation" float->positional tile-elevation))
    (define temperature-edit (climate-edit "temperature" float->positional tile-temperature))
    (define insolation-edit (climate-edit "insolation" float->exponential tile-insolation))
    (define aridity-edit (climate-edit "aridity" float->positional tile-aridity))
    (define relative-humidity-edit (climate-edit "relative humidity" float->positional tile-relative-humidity))
    (define absolute-humidity-edit (climate-edit "absolute humidity" float->exponential tile-humidity))
    (define precipitation-edit (climate-edit "precipitation" float->exponential tile-precipitation))
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
        (send c update)))))
