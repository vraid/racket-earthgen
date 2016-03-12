#lang typed/racket

(require vraid/types)

(require/typed/provide "climate-data.rkt"
                       [#:struct tile-climate-data
                                 ([snow : float-get]
                                  [sunlight : float-get]
                                  [temperature : float-get]
                                  [humidity : float-get]
                                  [precipitation : float-get]
                                  [snow-set! : float-set!]
                                  [temperature-set! : float-set!]
                                  [sunlight-set! : float-set!]
                                  [humidity-set! : float-set!]
                                  [precipitation-set! : float-set!])]
                       [#:struct corner-climate-data
                                 ([river-flow : float-get]
                                  [river-flow-set! : float-set!])]
                       [#:struct edge-climate-data
                                 ([river-flow : float-get]
                                  [air-flow : float-get]
                                  [river-flow-set! : float-set!]
                                  [air-flow-set! : float-set!])]
                       [make-tile-climate-data (Integer -> tile-climate-data)]
                       [make-corner-climate-data (Integer -> corner-climate-data)]
                       [make-edge-climate-data (Integer -> edge-climate-data)])
