#lang typed/racket

(require vraid/struct
         vraid/math
         math/flonum
         "../terrain/terrain-structs.rkt"
         "../direct-access.rkt")

(provide (all-defined-out))

(struct/kw climate-parameters
           ([axial-tilt : Float]
            [seasons-per-cycle : Positive-Integer]
            [acceptable-delta : Float]
            [precipitation-factor : Float]
            [humidity-half-life-days : Float])
           #:transparent)

(: default-climate-parameters (-> climate-parameters))
(define (default-climate-parameters)
  (climate-parameters/kw
   #:acceptable-delta 0.01
   #:axial-tilt (/ pi 8.0)
   #:seasons-per-cycle 16
   #:precipitation-factor 1.0
   #:humidity-half-life-days 5.0))

(: time-of-year (Integer Integer -> Float))
(define (time-of-year seasons-per-cycle season)
  (fl (/ season seasons-per-cycle)))

(: solar-equator (Float Float -> Float))
(define (solar-equator axial-tilt time-of-year)
  (* (sin (* tau time-of-year))
     axial-tilt))

(: planet-time-of-year (planet-climate -> Float))
(define (planet-time-of-year planet)
  (time-of-year (climate-parameters-seasons-per-cycle (planet-climate-parameters planet))
                (planet-climate-season planet)))

(: planet-solar-equator (planet-climate -> Float))
(define (planet-solar-equator planet)
  (solar-equator (climate-parameters-axial-tilt (planet-climate-parameters planet))
                 (planet-time-of-year planet)))

(vector-struct tile-climate-data
               ([sunlight : flvec Float]
                [temperature : flvec Float]
                [humidity : flvec Float]
                [precipitation : flvec Float]
                [snow : flvec Float]
                [leaf-area-index : flvec Float]))

(vector-struct corner-climate-data
               ([river-flow : flvec Float]))

(vector-struct edge-climate-data
               ([river-flow : flvec Float]
                [air-flow : flvec Float]))

(struct/kw planet-climate planet-terrain
           ([parameters : climate-parameters]
            [season : Integer]
            [tile : tile-climate-data]
            [corner : corner-climate-data]
            [edge : edge-climate-data]))

(direct-access planet-climate tile tile-climate-data
               ([snow Float]
                [sunlight Float]
                [temperature Float]
                [humidity Float]
                [precipitation Float]
                [leaf-area-index Float]))

(direct-access planet-climate corner corner-climate-data
               ([river-flow Float]))

(direct-access planet-climate edge edge-climate-data
               ([river-flow Float]
                [air-flow Float]))
