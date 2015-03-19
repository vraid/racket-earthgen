#lang typed/racket

(provide (all-defined-out)
         (all-from-out "climate-data-structs.rkt"))

(require vraid/struct
         vraid/types
         vraid/math
         math/flonum
         "../terrain/terrain-structs.rkt"
         "climate-data-structs.rkt"
         "../direct-access.rkt")

(struct/kw: climate-parameters
            ([axial-tilt : Flonum]
             [seasons-per-cycle : Positive-Integer]
             [acceptable-delta : Flonum])
            #:transparent)

(: default-climate-parameters (-> climate-parameters))
(define (default-climate-parameters)
  (climate-parameters/kw
   #:acceptable-delta 0.01
   #:axial-tilt (/ pi 8.0)
   #:seasons-per-cycle 16))

(: planet-time-of-year (planet-climate -> flonum))
(define (planet-time-of-year planet)
  (fl (/ (planet-climate-season planet)
         (climate-parameters-seasons-per-cycle (planet-climate-parameters planet)))))

(: planet-solar-equator (planet-climate -> flonum))
(define (planet-solar-equator planet)
  (* (sin (* tau (planet-time-of-year planet)))
     (climate-parameters-axial-tilt (planet-climate-parameters planet))))

(struct/kw: planet-climate planet-terrain
            ([parameters : climate-parameters]
             [season : integer]
             [tile : tile-climate-data]
             [edge : edge-climate-data]))

(direct-access planet-climate tile tile-climate-data
               ([sunlight flonum]
                [temperature flonum]
                [humidity flonum]
                [precipitation flonum]
                [snow-cover flonum]))

(direct-access planet-climate edge edge-climate-data
               ([river-flow flonum]
                [air-flow flonum]))
