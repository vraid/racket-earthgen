#lang typed/racket

(provide (all-defined-out))

(require math/flonum
         "../climate.rkt")

(: supported-vegetation (Float Float Float -> Float))
(define (supported-vegetation sunlight temperature humidity)
  (if (or (zero? sunlight)
          (below-freezing-temperature? temperature))
      0.0
      (fl* 3.0 (fl* (max 0.0 (fl- 1.0 (aridity temperature humidity)))
                    sunlight))))

(: vegetation-cover (Float -> Float))
(define (vegetation-cover vegetation)
  (let* ([half-cover 1.0]
         [k (fl/ (log 0.5) half-cover)])
    (fl- 1.0 (flexp (fl* k vegetation)))))
