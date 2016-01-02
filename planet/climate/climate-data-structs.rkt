#lang typed/racket

(require vraid/types)

(require/typed/provide "climate-data.rkt"
                       [#:struct tile-climate-data
                                 ([snow : flonum-get]
                                  [sunlight : flonum-get]
                                  [temperature : flonum-get]
                                  [humidity : flonum-get]
                                  [precipitation : flonum-get]
                                  [snow-set! : flonum-set!]
                                  [temperature-set! : flonum-set!]
                                  [sunlight-set! : flonum-set!]
                                  [humidity-set! : flonum-set!]
                                  [precipitation-set! : flonum-set!])]
                       [#:struct corner-climate-data
                                 ([river-flow : flonum-get]
                                  [river-flow-set! : flonum-set!])]
                       [#:struct edge-climate-data
                                 ([river-flow : flonum-get]
                                  [air-flow : flonum-get]
                                  [river-flow-set! : flonum-set!]
                                  [air-flow-set! : flonum-set!])]
                       [make-tile-climate-data (Integer -> tile-climate-data)]
                       [make-corner-climate-data (Integer -> corner-climate-data)]
                       [make-edge-climate-data (Integer -> edge-climate-data)])
