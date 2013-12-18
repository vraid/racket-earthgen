#lang typed/racket

(provide (struct-out tile-data)
         (struct-out corner-data)
         (struct-out edge-data)
         make-tile-data
         make-corner-data
         make-edge-data)

(require "types.rkt")

(require/typed "planet-data-structs.rkt"
               [#:struct tile-data
                         ([elevation : (index -> Flonum)]
                          [water-level : (index -> Flonum)]
                          [temperature : (index -> Flonum)]
                          [humidity : (index -> Flonum)]
                          [precipitation : (index -> Flonum)]
                          [elevation-set! : (index Flonum -> Void)]
                          [water-level-set! : (index Flonum -> Void)]
                          [temperature-set! : (index Flonum -> Void)]
                          [humidity-set! : (index Flonum -> Void)]
                          (precipitation-set! : (index Flonum -> Void)))]
               [#:struct corner-data
                         ([elevation : (index -> Flonum)]
                          [river-direction : (index -> Fixnum)]
                          [elevation-set! : (index Flonum -> Void)]
                          [river-direction-set! : (index Fixnum -> Void)])]
               [#:struct edge-data
                         ([has-river? : (index -> Boolean)]
                          [river-flow : (index -> Flonum)]
                          [has-river?-set! : (index Boolean -> Void)]
                          [river-flow-set! : (index Flonum -> Void)])]
               [make-tile-data (Integer -> tile-data)]
               [make-corner-data (Integer -> corner-data)]
               [make-edge-data (Integer -> edge-data)])
