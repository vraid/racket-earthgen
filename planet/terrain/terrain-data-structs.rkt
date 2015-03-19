#lang typed/racket

(require vraid/types)

(require/typed/provide "terrain-data.rkt"
                       [#:struct tile-terrain-data
                                 ([elevation : flonum-get]
                                  [elevation-set! : flonum-set!])]
                       [#:struct corner-terrain-data
                                 ([elevation : flonum-get]
                                  [river-direction : integer-get]
                                  [elevation-set! : flonum-set!]
                                  [river-direction-set! : integer-set!])]
                       [make-tile-terrain-data (Integer -> tile-terrain-data)]
                       [make-corner-terrain-data (Integer -> corner-terrain-data)])
