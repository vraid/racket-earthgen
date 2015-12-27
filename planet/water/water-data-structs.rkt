#lang typed/racket

(require vraid/types)

(require/typed/provide "water-data.rkt"
                       [#:struct tile-water-data
                                 ([water-level : flonum-get]
                                  [water-level-set! : flonum-set!])]
                       [#:struct corner-water-data
                                 ([river-direction : integer-get]
                                  [river-direction-set! : integer-set!])]
                       [make-tile-water-data (Integer -> tile-water-data)]
                       [make-corner-water-data (Integer -> corner-water-data)])
