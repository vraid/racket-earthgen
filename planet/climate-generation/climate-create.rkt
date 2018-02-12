#lang typed/racket

(require vraid/util
         "../grid-base.rkt"
         "../geometry.rkt"
         "../terrain.rkt"
         "../climate.rkt"
         "defaults.rkt"
         "climate-create-base.rkt")

(provide static-climate
         singular-climate
         climate-next
         climate-parameters/kw
         default-climate-parameters
         default-wind)

(: singular-climate (climate-parameters (String -> Any) -> (planet-terrain -> planet-climate)))
(define ((singular-climate param feedback) planet)
  (let* ([initial ((climate/closed-season param planet feedback) 0)]
         [p ((climate/closed-season param planet feedback) 0)])
    (generate-climate! param initial p feedback)
    p))

(: static-climate (climate-parameters planet-terrain (String -> Any) -> (planet-terrain -> planet-climate)))
(define (static-climate param planet feedback)
  (let* ([season-count (climate-parameters-seasons-per-cycle param)]
         [initial ((climate/closed-season param planet feedback) 0)]
         [v (build-vector season-count
                          (λ ([n : Integer])
                            (delay (let ([p ((climate/closed-season param planet feedback) n)])
                                     (generate-climate! param initial p feedback)
                                     p))))])
    (λ ([planet : planet-terrain])
      (let ([season (if (planet-climate? planet)
                        (modulo (+ 1 (planet-climate-season planet))
                                season-count)
                        0)])
        (force (vector-ref v season))))))

(: climate/closed-season (climate-parameters planet-terrain (String -> Any) -> (Integer -> planet-climate)))
(define ((climate/closed-season par planet feedback) season)
  (let* ([tile-count (tile-count planet)]
         [corner-count (corner-count planet)]
         [edge-count (edge-count planet)]
         [axial-tilt (climate-parameters-axial-tilt par)]
         [tile-latitude (curry tile-latitude planet)]
         [seasons-per-cycle (climate-parameters-seasons-per-cycle par)]
         [time-of-year (time-of-year seasons-per-cycle season)]
         [solar-equator (solar-equator axial-tilt time-of-year)]
         [tile-sunlight (build-flvector-accessor
                         tile-count
                         (λ ([n : Integer])
                           (sunlight
                            solar-equator
                            (tile-latitude n))))]
         [tile-temperature (build-flvector-accessor
                            tile-count
                            (default-temperature planet (vector-accessor-get tile-sunlight)))]
         [tile-snow-cover (build-flvector-accessor
                           tile-count
                           (default-snow-cover planet (vector-accessor-get tile-temperature)))])
    (planet-climate/kw
     #:planet-terrain planet
     #:parameters par
     #:season season
     #:tile (tile-climate-data/accessors
             #:sunlight tile-sunlight
             #:temperature tile-temperature
             #:humidity (zeros tile-count)
             #:precipitation (zeros tile-count)
             #:snow tile-snow-cover
             #:leaf-area-index (zeros tile-count))
     #:corner (corner-climate-data/accessors
               #:river-flow (zeros corner-count))
     #:edge (edge-climate-data/accessors
             #:river-flow (zeros edge-count)
             #:air-flow (zeros edge-count)))))

(: climate-next (climate-parameters (String -> Any) -> (planet-climate -> planet-climate)))
(define ((climate-next par feedback) prev)
  (let ([p (initial-values par prev)])
    (generate-climate! par prev p feedback)
    p))
