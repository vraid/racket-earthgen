#lang typed/racket

(provide climate-next
         climate-parameters/kw
         default-climate-parameters
         default-wind)

(require math/flonum
         vraid/types
         vraid/flow
         vraid/math
         vraid/typed-array
         "planet.rkt"
         "planet-typed-data-structs.rkt"
         "climate-structs.rkt"
         "river-generation.rkt"
         "math/wind.rkt")

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
  (let ([init-tile-array (init-array (tile-count p))]
        [init-corner-array (init-array (corner-count p))])
    (init-tile-array (tile-data-elevation-set! (planet-tile p))
                     (curry tile-elevation prev))
    (init-tile-array (tile-data-water-level-set! (planet-tile p))
                     (curry tile-water-level prev))
    (init-corner-array (corner-data-elevation-set! (planet-corner p))
                       (curry corner-elevation prev))
    (init-corner-array (corner-data-river-direction-set! (planet-corner p))
                       (lambda: ([n : integer])
                         ((corner-data-river-direction (planet-corner prev)) n)))))

(: sunlight (flonum flonum -> flonum))
(define (sunlight solar-equator latitude)
  (max 0.0 (flcos (fl- solar-equator latitude))))

(: default-temperature (planet integer -> Flonum))
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

(: planet/default-population (planet -> planet))
(define (planet/default-population p)
  (struct-copy planet p
               [has-climate? #t]
               [land-ratio (lambda: ([n : integer])
                             (lambda: ([key : land-type])
                               (match key
                                 ['unclaimed 1.0]
                                 [_ #f])))]
               [population (lambda: ([n : integer])
                             (lambda: ([key : population-type])
                               #f))]))

(: climate-first (climate-parameters planet -> planet))
(define (climate-first par prev)
  (let ([p (struct-copy planet prev
            [has-climate? #t]
            [climate-parameters par]
            [climate-variables initial-climate-variables]
            [tile (make-tile-data (tile-count prev))]
            [corner (make-corner-data (corner-count prev))]
            [edge (make-edge-data (edge-count prev))]
            [land-ratio (lambda: ([n : integer])
                           (lambda: ([key : land-type])
                             (match key
                               ['unclaimed 1.0]
                               [_ #f])))]
            [population (lambda: ([n : integer])
                           (lambda: ([key : population-type])
                             #f))])])
    (copy-geography! prev p)
    (generate-rivers! p)
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
  ([tile-humidity : FlVector]))

(: make-climate-data (planet -> climate-data))
(define (make-climate-data p)
  (climate-data
   (make-flvector (tile-count p) 0.0)))

(struct: wind
  ([origin : integer]
   [scale : flonum]))

(: set-wind! (planet -> Void))
(define (set-wind! p)
      (let* ([tropical-equator (* 0.5 (climate-variables-solar-equator (planet-climate-variables p)))]
             [edge-wind (make-flvector (edge-count p) 0.0)]
             [init-edge-array (init-array (edge-count p))]
             [add (lambda: ([n : integer]
                            [a : Flonum])
                    (flvector-set! edge-wind n (+ a (flvector-ref edge-wind n))))]
             [tile-edge-sign (let ([v (build-vector (* 6 (tile-count p))
                                                        (lambda: ([n : integer])
                                                          (let ([i (modulo n 6)]
                                                                [n (inexact->exact (floor (/ n 6)))])
                                                            (edge-tile-sign p (tile-edge p n i) n))))])
                                   (lambda: ([p : planet]
                                             [n : integer]
                                             [i : integer])
                                     (vector-ref v (+ i (* 6 n)))))])
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
        (init-edge-array (edge-data-air-flow-set! (planet-edge p))
                         (lambda: ([n : integer]) (flvector-ref edge-wind n)))))

(: generate-climate! (climate-parameters planet planet -> Void))
(define (generate-climate! par prev p)
  (let* ([tile-water? (let ([v (build-vector (tile-count p)
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
    (define (climate-iterate!)
      (let* ([edge-wind (let ([winds (build-vector
                                      (edge-count p)
                                      (lambda: ([n : integer])
                                        (let ([scale (* ((edge-data-air-flow (planet-edge p)) n)
                                                        (edge-length n))])
                                          (wind
                                           (edge-tile p n (if (> 0.0 scale) 0 1))
                                           (abs scale)))))])
                          (lambda: ([n : integer])
                            (vector-ref winds n)))]
             [wind-list/filter (lambda: ([f : (integer integer -> Boolean)])
                                 (let ([v (build-vector
                                           (tile-count p)
                                           (lambda: ([n : integer])
                                             (map edge-wind
                                                  (filter (curry f n)
                                                          (grid-tile-edge-list p n)))))])
                                   (lambda: ([n : integer])
                                     (vector-ref v n))))]
             [incoming-winds (wind-list/filter (lambda: ([n : integer]
                                                         [e : integer])
                                                 (not (= n (wind-origin (edge-wind e))))))]
             [outgoing-winds (wind-list/filter (lambda: ([n : integer]
                                                         [e : integer])
                                                 (= n (wind-origin (edge-wind e)))))]
             [total-wind (lambda: ([ls : (Listof wind)])
                           (foldl + 0.0 (map wind-scale ls)))]
             [total-incoming-wind (lambda: ([n : integer])
                                    (total-wind (incoming-winds n)))]
             [total-outgoing-wind (lambda: ([n : integer])
                                    (total-wind (outgoing-winds n)))]
             [absolute-incoming-humidity (lambda: ([tile-humidity : (integer -> flonum)]
                                                   [n : integer])
                                           (for/fold: ([humidity : Flonum 0.0])
                                             ([w (incoming-winds n)])
                                             (+ humidity
                                                (* (tile-humidity (wind-origin w))
                                                   (wind-scale w)))))])
        (: iterate! (climate-data climate-data Real -> climate-data))
        (define (iterate! to from delta)
          (if (< delta (climate-parameters-acceptable-delta par))
              from
              (let* ([set-tile-humidity! (lambda: ([n : integer]
                                                   [a : Flonum])
                                           (flvector-set! (climate-data-tile-humidity to) n a))]
                     [tile-humidity (lambda: ([n : integer])
                                      (flvector-ref (climate-data-tile-humidity from) n))])
                (for ([n (tile-count p)])
                  (set-tile-humidity! n (if (tile-water? p n)
                                            (saturation-humidity (tile-temperature p n))
                                            (min (saturation-humidity (tile-temperature p n))
                                                 (let ([outgoing (total-outgoing-wind n)])
                                                   (if (zero? outgoing)
                                                       (saturation-humidity (tile-temperature p n))
                                                       (/ (absolute-incoming-humidity tile-humidity n)
                                                          outgoing)))))))
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
                                                         (flvector-ref (climate-data-tile-humidity climate-values) n)))
            (for ([n (tile-count p)])
              ((tile-data-precipitation-set! (planet-tile p)) n
                                                              (let ([outgoing (* (tile-humidity p n)
                                                                                 (total-outgoing-wind n))]
                                                                    [incoming (absolute-incoming-humidity (curry tile-humidity p) n)])
                                                                (max 0.0 (/ (* 200.0 (- incoming outgoing))
                                                                            (tile-area n))))))))))
    (define (set-river-flow!)
      (: river-outflow (integer -> flonum))
      (define river-outflow
        (let ([v (build-flvector (tile-count p)
                                 (lambda: ([n : integer])
                                   (/ (* (tile-area n)
                                         (tile-precipitation p n))
                                      (tile-edge-count n))))])
          (lambda: ([n : integer])
            (flvector-ref v n))))
      (define river-flow-vec (make-flvector (corner-count p) 0.0))
      (: river-flow (integer -> flonum))
      (define (river-flow n)
        (flvector-ref river-flow-vec n))
      (: visit-river (river -> Void))
      (define (visit-river r)
        (for ([n (river-sources r)])
          (visit-river n))
        (let ([n (river-location r)])
          (flvector-set! river-flow-vec
                         n
                         (+ (for/fold: ([sum : flonum 0.0])
                              ([i 3])
                              (+ sum (river-outflow (corner-tile p n i))))
                            (for/fold: ([sum : flonum 0.0])
                              ([i (river-sources r)])
                              (+ sum (river-flow (river-location i)))))))
        (when-let* ([n (river-location r)]
                    [dir (corner-river-direction p n)])
                   ((edge-data-river-flow-set! (planet-edge p)) (corner-edge p n dir) (river-flow n))))
      (for ([r (planet-rivers p)])
        (visit-river r)))
    (set-wind! p)
    (climate-iterate!)
    (set-river-flow!)
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
      (let* ([p (struct-copy planet prev
                 [has-climate? #t]
                 [climate-parameters par]
                 [climate-variables (next-climate-variables par (planet-climate-variables prev))]
                 [tile (make-tile-data (tile-count prev))]
                 [corner (make-corner-data (corner-count prev))]
                 [edge (make-edge-data (edge-count prev))]
                 [land-ratio (lambda: ([n : integer])
                                (lambda: ([key : land-type])
                                  (match key
                                    ['unclaimed 1.0]
                                    [_ #f])))]
                 [population (lambda: ([n : integer])
                                (lambda: ([key : population-type])
                                  #f))])]
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
