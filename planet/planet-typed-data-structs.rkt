#lang typed/racket

(require vraid/types)

(define-type boolean-access (index -> Boolean))
(define-type boolean-set! (index Boolean -> Void))
(define-type fixnum-access (index -> Fixnum))
(define-type fixnum-set! (index Fixnum -> Void))
(define-type flonum-access (index -> Flonum))
(define-type flonum-set! (index Flonum -> Void))

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
                                  [river-direction : fixnum-access]
                                  [elevation-set! : flonum-set!]
                                  [river-direction-set! : fixnum-set!])]
                       [#:struct edge-data
                                 ([river-flow : flonum-access]
                                  [surface-air-flow : flonum-access]
                                  [troposphere-air-flow : flonum-access]
                                  [river-flow-set! : flonum-set!]
                                  [surface-air-flow-set! : flonum-set!]
                                  [troposphere-air-flow-set! : flonum-set!])]
                       [make-tile-data (Integer -> tile-data)]
                       [make-corner-data (Integer -> corner-data)]
                       [make-edge-data (Integer -> edge-data)])
