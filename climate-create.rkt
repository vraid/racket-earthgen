#lang typed/racket

(require math/flonum
         "types.rkt"
         "planet.rkt"
         "grid.rkt"
         "climate-structs.rkt"
         "vector3.rkt"
         "parallel-util.rkt")

(provide climate-next
         climate-parameters)

(: temperature-first (planet index -> Flonum))
(define (temperature-first p n)
  (let ([light (cos (tile-latitude p n))])
    (+ 200.0
       (* 130.0 light))))

(: climate-first (climate-parameters planet -> planet))
(define (climate-first par prev)
  (let ([p (planet
            (planet-grid prev)
            true
            (make-tile-data (tile-count prev))
            (make-corner-data (corner-count prev))
            (make-edge-data (edge-count prev)))])
    (begin
      (for ([n (tile-count p)])
        ((tile-data-elevation-set! (planet-tile p)) n
                                                    (tile-elevation prev n))
        ((tile-data-temperature-set! (planet-tile p)) n
                                                      (temperature-first p n)))
      (for ([n (corner-count p)])
        ((corner-data-elevation-set! (planet-corner p)) n
                                                        (corner-elevation prev n)))
      p)))

(: climate-next (climate-parameters planet -> planet))
(define (climate-next par prev)
  (if (not (planet-has-climate? prev))
      (climate-first par prev)
      prev))
