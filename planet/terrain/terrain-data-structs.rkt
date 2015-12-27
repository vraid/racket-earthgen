#lang typed/racket

(require vraid/types)

(require/typed/provide "terrain-data.rkt"
                       [#:struct tile-terrain-data
                                 ([elevation : flonum-get]
                                  [elevation-set! : flonum-set!])]
                       [#:struct corner-terrain-data
                                 ([elevation : flonum-get]
                                  [elevation-set! : flonum-set!])]
                       [make-tile-terrain-data (Integer -> tile-terrain-data)]
                       [make-corner-terrain-data (Integer -> corner-terrain-data)])
