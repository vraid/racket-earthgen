#lang typed/racket

(require math/flonum
         "types.rkt"
         "planet.rkt"
         "grid.rkt"
         "climate-structs.rkt"
         "wind.rkt"
         "vector3.rkt"
         "parallel-util.rkt")

(provide climate-next
         climate-parameters
         climate-default-parameters)

(: climate-default-parameters (-> climate-parameters))
(define (climate-default-parameters)
  (climate-parameters 12))

(: temperature-first (planet index -> Flonum))
(define (temperature-first p n)
  (let ([light (cos (tile-latitude p n))])
    (+ 200.0
       (* 130.0 light))))

(define zero-wind (wind 0.0 0.0))

(define (default-wind)
  zero-wind)

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
        ((tile-data-elevation-set! (planet-tile p)) n (tile-elevation prev n))
        ((tile-data-temperature-set! (planet-tile p)) n (temperature-first p n)))
      (for ([n (corner-count p)])
        ((corner-data-elevation-set! (planet-corner p)) n (corner-elevation prev n)))
      (for ([n (edge-count p)])
        (void))
      p)))

(: initial-values (climate-parameters planet -> planet))
(define (initial-values par prev)
  (if (not (planet-has-climate? prev))
      (climate-first par prev)
      (let ([p (planet
                (planet-grid prev)
                true
                (make-tile-data (tile-count prev))
                (make-corner-data (corner-count prev))
                (make-edge-data (edge-count prev)))])
        (begin
          (for ([n (tile-count p)])
            ((tile-data-elevation-set! (planet-tile p)) n (tile-elevation prev n))
            ((tile-data-temperature-set! (planet-tile p)) n (tile-temperature prev n)))
          (for ([n (corner-count p)])
            ((corner-data-elevation-set! (planet-corner p)) n (corner-elevation prev n)))
          (for ([n (edge-count p)])
            (void))
          p))))

(struct: climate-values
  ([tiles : tile-values]))

(struct: tile-values
  ([humidity : FlVector]
   [precipitation : FlVector]))

(: climate-next (climate-parameters planet -> planet))
(define (climate-next par prev)
  (let* ([p (initial-values par prev)])
    (define (set-wind!)
      (let* ([tile-wind (make-vector (tile-count prev)
                                     (default-wind))]
             [edge-wind (make-vector (edge-count prev)
                                     0.0)])
        (for ([n (edge-count p)])
          ((edge-data-surface-air-flow-set! (planet-edge p)) n (vector-ref edge-wind n)))))
    (define (climate-iterate! p)
      (: iterate! (climate-values climate-values Flonum -> climate-values))
      (define (iterate! to from delta)
        to)
      (begin
        (void)))
    (begin
      (set-wind!)
      (climate-iterate! p)
      p)))
