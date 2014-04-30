#lang typed/racket

(provide climate-next
         climate-parameters/kw
         default-climate-parameters
         default-wind)

(require math/flonum
         "types.rkt"
         "math.rkt"
         "planet.rkt"
         "planet-typed-data-structs.rkt"
         "typed-arrays.rkt"
         "climate-structs.rkt"
         "flvector3.rkt"
         "wind.rkt")

(: next-climate-variables (climate-parameters climate-variables -> climate-variables))
(define (next-climate-variables par prev)
  (let* ([season (+ 1 (climate-variables-season prev))]
         [seasons-per-cycle (climate-parameters-seasons-per-cycle par)]
         [time-of-year (exact->inexact (/ (modulo season seasons-per-cycle)
                                          seasons-per-cycle))]
         [solar-equator (* (sin (* tau time-of-year))
                           (climate-parameters-axial-tilt par))])
    (climate-variables/kw
     #:season season
     #:time-of-year time-of-year
     #:solar-equator solar-equator)))

(: copy-geography! (planet planet -> Void))
(define (copy-geography! prev p)
  (let ([init-tile-array (init-array (tile-count p))])
    (init-tile-array (tile-data-elevation-set! (planet-tile p))
                     (curry tile-elevation prev))
    (init-tile-array (tile-data-water-level-set! (planet-tile p))
                     (curry tile-water-level prev))
    ((init-array (corner-count p)) (corner-data-elevation-set! (planet-corner p))
                                   (curry corner-elevation prev))))

(: sunlight (flonum flonum -> flonum))
(define (sunlight solar-equator latitude)
  (max 0.0 (flcos (fl- solar-equator latitude))))

(: default-temperature (planet index -> Flonum))
(define (default-temperature p n)
  (-
   (+ 200.0
      (* 130.0 (if (tile-land? p n)
                   (tile-sunlight p n)
                   (sunlight 0.0 (tile-latitude p n)))))
   (max 0.0
        (temperature-lapse (tile-elevation p n)))))

(: default-snow-cover (planet integer -> Flonum))
(define (default-snow-cover p n)
  (let ([temperature (tile-temperature p n)])
    (if (tile-water? p n)
        0.0
        (if (below-freezing-temperature? temperature)
            1.0
            0.0))))

(: default-pressure-gradient-force (Flonum Flonum -> flvector3))
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

(: default-wind (Flonum planet integer -> flvector3))
(define (default-wind tropical-equator p n)
  (prevailing-wind p
                   (tile-coordinates p n)
                   (default-pressure-gradient-force tropical-equator (tile-latitude p n))
                   (tile-surface-friction p n)))

(: climate-first (climate-parameters planet -> planet))
(define (climate-first par prev)
  (let ([p (planet/kw
            #:grid (planet-grid prev)
            #:has-climate? true
            #:climate-parameters par
            #:climate-variables initial-climate-variables
            #:tile (make-tile-data (tile-count prev))
            #:corner (make-corner-data (corner-count prev))
            #:edge (make-edge-data (edge-count prev)))])
    (copy-geography! prev p)
    (let ([init-tile-array (init-array (tile-count p))])
      (init-tile-array (tile-data-sunlight-set! (planet-tile p))
                       (lambda: ([n : integer])
                         (sunlight
                          (climate-variables-solar-equator (planet-climate-variables p))
                          (tile-latitude p n))))
      (init-tile-array (tile-data-temperature-set! (planet-tile p))
                       (curry default-temperature p))
      (init-tile-array (tile-data-snow-cover-set! (planet-tile p))
                       (curry default-snow-cover p)))
    p))

(: climate-next (climate-parameters planet -> planet))
(define (climate-next par prev)
  (let ([p (initial-values par prev)])
    (generate-climate! par prev p)
    p))

(struct: climate-data
  ([tile-humidity : FlVector]
   [tile-precipitation : FlVector]))

(: make-climate-data (planet -> climate-data))
(define (make-climate-data p)
  (let ([tile-vector (thunk (make-flvector (tile-count p) 0.0))])
    (climate-data
     (tile-vector)
     (tile-vector))))

(: generate-climate! (climate-parameters planet planet -> Void))
(define (generate-climate! par prev p)
  (let* ([tropical-equator (* 0.5 (climate-variables-solar-equator (planet-climate-variables p)))]
         [tile-edge-sign (let ([v (build-vector (* 6 (tile-count p))
                                                (lambda: ([n : integer])
                                                  (let ([i (modulo n 6)]
                                                        [n (inexact->exact (floor (/ n 6)))])
                                                    (edge-tile-sign p (tile-edge p n i) n))))])
                           (lambda: ([p : planet]
                                     [n : integer]
                                     [i : integer])
                             (vector-ref v (+ i (* 6 n)))))]                               
         [tile-water? (let ([v (build-vector (tile-count p)
                                             (lambda: ([n : integer])
                                               (tile-water? p n)))])
                        (lambda: ([p : planet]
                                  [n : Integer])
                          (vector-ref v n)))]
         [tile-land? (lambda: ([p : planet]
                               [n : Integer])
                       (not (tile-water? p n)))]
         [tile-areas (build-flvector (tile-count p) (curry tile-area p))]
         [tile-area (lambda: ([n : integer])
                      (flvector-ref tile-areas n))]
         [edge-lengths (build-flvector (edge-count p) (curry edge-length p))]
         [edge-length (lambda: ([n : integer])
                        (flvector-ref edge-lengths n))]
         [edge-tile-distances (build-flvector (edge-count p) (curry edge-tile-distance p))]
         [tile-tile-distance (lambda: ([p : planet]
                                       [n : integer]
                                       [i : integer])
                               (flvector-ref edge-tile-distances (tile-edge p n i)))])
    (define edge-wind (make-flvector (edge-count p) 0.0))
    (define (set-wind!)
      (let* ([init-edge-array (init-array (edge-count p))]
             [add (lambda: ([n : index]
                            [a : Flonum])
                    (flvector-set! edge-wind n (+ a (flvector-ref edge-wind n))))])
        (for ([n (tile-count p)])
          (let* ([wind-vector (default-wind tropical-equator p n)]
                 [tile-vector (tile-coordinates p n)]
                 [negative-wind-vector (flvector3-negative wind-vector)]
                 [tile-wind (build-flvector (tile-edge-count n)
                                            (lambda (i)
                                              (let ([neighbour-vector (flvector3-normal (flvector3-rejection tile-vector
                                                                                                             (tile-coordinates p (tile-tile p n i))))])
                                                (* (if (< (flvector3-distance-squared neighbour-vector wind-vector)
                                                          (flvector3-distance-squared neighbour-vector negative-wind-vector))
                                                       1.0
                                                       -1.0)
                                                   (flvector3-length (flvector3-projection neighbour-vector wind-vector))))))])
            (for ([i (tile-edge-count n)])
              (let ([e (tile-edge p n i)])
                (add e (* 0.5
                          (tile-edge-sign p n i)
                          (flvector-ref tile-wind i)))))))
        (init-edge-array (edge-data-surface-air-flow-set! (planet-edge p))
                         (lambda: ([n : index]) (flvector-ref edge-wind n)))))
    (define (climate-iterate!)
      (let* ([edge-winds (build-flvector (edge-count p)
                                         (lambda: ([n : integer])
                                           (* (flvector-ref edge-wind n)
                                              (edge-length n))))]
             [edge-wind (lambda: ([n : integer])
                          (flvector-ref edge-winds n))]
             [tile-convections (build-flvector (tile-count p)
                                               (lambda: ([n : integer])
                                                 (fl (apply + (map (lambda: ([e : integer])
                                                                     (* (edge-wind e)
                                                                        (edge-tile-sign p e n)))
                                                                   (grid-tile-edge-list (planet-grid p) n))))))]
             [tile-convection (lambda: ([n : integer])
                                (flvector-ref tile-convections n))]
             [incoming-winds (build-flvector (tile-count p)
                                             (lambda: ([n : integer])
                                               (for/fold: ([wind : Flonum 0.0])
                                                 ([i (tile-edge-count n)])
                                                 (+ wind
                                                    (let ([e (tile-edge p n i)])
                                                      (max 0.0
                                                           (fl* (edge-wind e)
                                                                (edge-tile-sign p e n))))))))]
             [total-incoming-wind (lambda: ([n : integer])
                                    (flvector-ref incoming-winds n))])
        (: iterate! (climate-data climate-data Real -> climate-data))
        (define (iterate! to from delta)
          (if (< delta (climate-parameters-acceptable-delta par))
              from
              (let* ([set-tile-humidity! (lambda: ([n : integer]
                                                   [a : Flonum])
                                           (flvector-set! (climate-data-tile-humidity to) n a))]
                     [tile-humidity (lambda: ([n : integer])
                                      (flvector-ref (climate-data-tile-humidity from) n))]
                     [absolute-incoming-humidity (lambda: ([n : integer])
                                                   (if (zero? (total-incoming-wind n))
                                                       0.0
                                                       (fl/
                                                        (for/fold: ([humidity : Flonum 0.0])
                                                          ([i (tile-edge-count n)])
                                                          (fl+ humidity
                                                               (fl* (tile-humidity (tile-tile p n i))
                                                                    (max 0.0 (fl* (edge-wind (tile-edge p n i))
                                                                                  (tile-edge-sign p n i))))))
                                                        (total-incoming-wind n))))])
                (for ([n (tile-count p)])
                  (set-tile-humidity! n (if (tile-water? p n)
                                            (saturation-humidity (tile-temperature p n))
                                            (min (saturation-humidity (tile-temperature p n))
                                                 (absolute-incoming-humidity n)))))
                (iterate! from to (apply max (map (lambda: ([n : Integer])
                                                    (let ([current (flvector-ref (climate-data-tile-humidity to) n)]
                                                          [previous (flvector-ref (climate-data-tile-humidity from) n)])
                                                      (if (zero? current)
                                                          0.0
                                                          (if (zero? previous)
                                                              1.0
                                                              (flabs
                                                               (fl/ (fl- current
                                                                         previous)
                                                                    previous))))))
                                                  (range (tile-count p))))))))
        (let ([from (make-climate-data p)])
          (for ([n (tile-count p)])
            (flvector-set! (climate-data-tile-humidity from) n (if (tile-water? p n)
                                                                   (saturation-humidity (tile-temperature p n))
                                                                   0.0)))
          (let ([climate-values (iterate! (make-climate-data p)
                                          from
                                          1.0)])
            (for ([n (tile-count p)])
              ((tile-data-humidity-set! (planet-tile p)) n
                                                         (flvector-ref (climate-data-tile-humidity climate-values) n)))))))
    (set-wind!)
    (climate-iterate!)
    (for ([n (tile-count p)])
      ((tile-data-vegetation-set! (planet-tile p)) n
                                                   (if (tile-water? p n)
                                                       0.0
                                                       (supported-vegetation
                                                        (tile-sunlight p n)
                                                        (tile-temperature p n)
                                                        (tile-humidity p n)))))
    (void)))

(: initial-values (climate-parameters planet -> planet))
(define (initial-values par prev)
  (if (not (planet-has-climate? prev))
      (climate-first par prev)
      (let* ([p (planet/kw
                 #:grid (planet-grid prev)
                 #:has-climate? true
                 #:climate-parameters par
                 #:climate-variables (next-climate-variables par (planet-climate-variables prev))
                 #:tile (make-tile-data (tile-count prev))
                 #:corner (make-corner-data (corner-count prev))
                 #:edge (make-edge-data (edge-count prev)))]
             [init-tile-array (init-array (tile-count p))]
             [init-corner-array (init-array (corner-count p))]
             [init-edge-array (init-array (edge-count p))])
        (copy-geography! prev p)
        (let ([init-tile-array (init-array (tile-count p))])
          (init-tile-array (tile-data-sunlight-set! (planet-tile p))
                           (lambda: ([n : integer])
                             (sunlight
                              (climate-variables-solar-equator (planet-climate-variables p))
                              (tile-latitude p n))))
          (init-tile-array (tile-data-temperature-set! (planet-tile p))
                           (curry default-temperature p))
          (init-tile-array (tile-data-snow-cover-set! (planet-tile p))
                           (curry default-snow-cover p)))
        p)))
