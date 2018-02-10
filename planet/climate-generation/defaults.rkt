#lang typed/racket

(require math/flonum
         "../grid-base.rkt"
         "../geometry.rkt"
         "../terrain.rkt"
         "../climate.rkt")

(provide (all-defined-out))

(: default-temperature (planet-terrain (Integer -> Float) -> (Integer -> Float)))
(define ((default-temperature p tile-sunlight) n)
  (-
   (+ 220.0
      (* (/ 110.0 solar-constant)
         (if (tile-land? p n)
             (tile-sunlight n)
             (sunlight 0.0 (tile-latitude p n)))))
   (max 0.0
        (temperature-lapse (tile-elevation p n)))))

(: default-snow-cover (planet-terrain (Integer -> Float) -> (Integer -> Float)))
(define ((default-snow-cover p tile-temperature) n)
  (let ([temperature (tile-temperature n)])
    (if (tile-water? p n)
        0.0
        (if (below-freezing-temperature? temperature)
            1.0
            0.0))))

(: default-pressure-gradient-force (Float Float -> FlVector))
(define (default-pressure-gradient-force tropical-equator latitude)
  (let* ([c (fl/ (fl* 3.0 pi)
                 (fl+ (fl/ pi 2.0) (if (> tropical-equator latitude)
                                       tropical-equator
                                       (- tropical-equator))))]
         [pressure-deviation (fl/ 20.0 15000.0)]
         [pressure-derivate (fl/ (fl* pressure-deviation
                                      (flsin (fl* c (fl- latitude tropical-equator))))
                                 (if (< (fl- tropical-equator (fl/ (fl+ (fl/ pi 2.0) tropical-equator) 3.0))
                                        latitude
                                        (fl- tropical-equator (fl/ (fl- (fl/ pi 2.0) tropical-equator) 3.0)))
                                     3.0
                                     1.0))])
    (flvector 0.0 0.0 pressure-derivate)))

(: default-wind (Float planet-climate Integer -> FlVector))
(define (default-wind tropical-equator p n)
  (prevailing-wind p
                   (tile-coordinates p n)
                   (default-pressure-gradient-force tropical-equator (tile-latitude p n))
                   (tile-surface-friction p n)))
