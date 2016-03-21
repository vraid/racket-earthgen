#lang typed/racket

(provide (all-defined-out))

(require math/flonum
         "sunlight.rkt"
         "temperature.rkt"
         "../geometry/time.rkt")

(define leaf-insolation-scale (/ 6.0 (insolation (sunlight 0.0 0.0) 0.5)))
(define leaf-precipitation-scale (/ 6.0 (/ 0.0006 seconds-per-day)))

(: supported-leaf-area-index (Float Float Float -> Float))
(define (supported-leaf-area-index insolation temperature precipitation)
  (if (or (zero? insolation)
          (below-freezing-temperature? temperature))
      0.0
      (let ([precipitation-support (* leaf-precipitation-scale precipitation)]
            [insolation-support (* leaf-insolation-scale insolation)])
        (min precipitation-support
             insolation-support))))

(: vegetation-cover (Float -> Float))
(define (vegetation-cover vegetation)
  (let* ([half-cover 1.0]
         [k (fl/ (log 0.5) half-cover)])
    (fl- 1.0 (flexp (fl* k vegetation)))))
