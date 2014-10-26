#lang typed/racket

(require vraid/types)

(define-type boolean-access (integer -> Boolean))
(define-type boolean-set! (integer Boolean -> Void))
(define-type integer-access (integer -> integer))
(define-type integer-set! (integer integer -> Void))
(define-type flonum-access (integer -> Flonum))
(define-type flonum-set! (integer Flonum -> Void))

(require/typed/provide "planet-data-structs.rkt"
                       [#:struct tile-data
                                 ([elevation : flonum-access]
                                  [water-level : flonum-access]
                                  [sunlight : flonum-access]
                                  [temperature : flonum-access]
                                  [humidity : flonum-access]
                                  [precipitation : flonum-access]
                                  [snow-cover : flonum-access]
                                  [vegetation : flonum-access]
                                  [vertical-air-flow : flonum-access]
                                  [tropopause-altitude : flonum-access]
                                  [elevation-set! : flonum-set!]
                                  [water-level-set! : flonum-set!]
                                  [temperature-set! : flonum-set!]
                                  [sunlight-set! : flonum-set!]
                                  [humidity-set! : flonum-set!]
                                  [precipitation-set! : flonum-set!]
                                  [snow-cover-set! : flonum-set!]
                                  [vegetation-set! : flonum-set!]
                                  [vertical-air-flow-set! : flonum-set!]
                                  [tropopause-altitude-set! : flonum-set!])]
                       [#:struct corner-data
                                 ([elevation : flonum-access]
                                  [river-direction : integer-access]
                                  [elevation-set! : flonum-set!]
                                  [river-direction-set! : integer-set!])]
                       [#:struct edge-data
                                 ([river-flow : flonum-access]
                                  [air-flow : flonum-access]
                                  [river-flow-set! : flonum-set!]
                                  [air-flow-set! : flonum-set!])]
                       [make-tile-data (Integer -> tile-data)]
                       [make-corner-data (Integer -> corner-data)]
                       [make-edge-data (Integer -> edge-data)])
