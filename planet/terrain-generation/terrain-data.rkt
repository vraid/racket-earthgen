#lang typed/racket

(require math/flonum
         "../terrain-base.rkt")

(provide (all-defined-out))

(: get (All (A) ((Vectorof A) -> (Integer -> A))))
(define ((get v) n)
  (vector-ref v n))

(: set (All (A) ((Vectorof A) -> (Integer A -> Void))))
(define ((set v) n value)
  (vector-set! v n value))

(: flget (FlVector -> (Integer -> Float)))
(define ((flget v) n)
  (flvector-ref v n))

(: flset (FlVector -> (Integer Float -> Void)))
(define ((flset v) n value)
  (flvector-set! v n value))

(: make-tile-terrain-data (Integer (Integer -> Float) (Integer -> Float) -> tile-terrain-data))
(define (make-tile-terrain-data n elevation water-level)
  (let ([elevation (build-flvector n elevation)]
        [water-level (build-flvector n water-level)])
    (tile-terrain-data/kw #:elevation (flget elevation)
                          #:elevation-set! (flset elevation)
                          #:water-level (flget water-level)
                          #:water-level-set! (flset water-level))))

(: make-corner-terrain-data (Integer (Integer -> Float) (Integer -> (Option Integer)) -> corner-terrain-data))
(define (make-corner-terrain-data n elevation river-direction)
  (let ([elevation (build-flvector n elevation)]
        [river-direction (build-vector n river-direction)])
    (corner-terrain-data/kw #:elevation (flget elevation)
                            #:elevation-set! (flset elevation)
                            #:river-direction (get river-direction)
                            #:river-direction-set! (set river-direction))))
