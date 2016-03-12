#lang typed/racket

(require vraid/types)

(require/typed/provide "terrain-data.rkt"
                       [#:struct tile-terrain-data
                                 ([elevation : float-get]
                                  [elevation-set! : float-set!])]
                       [#:struct corner-terrain-data
                                 ([elevation : float-get]
                                  [elevation-set! : float-set!])]
                       [make-tile-terrain-data (Integer -> tile-terrain-data)]
                       [make-corner-terrain-data (Integer -> corner-terrain-data)])
