#lang typed/racket

(require vraid/flow
         vraid/math
         vraid/util
         math/flonum
         "../grid-base.rkt"
         "../geometry.rkt"
         "../terrain.rkt"
         "../climate.rkt"
         "defaults.rkt")

(provide (all-defined-out))

(define zeros
  (λ ([n : Integer])
    (make-flvector-accessor (make-flvector n 0.0))))

(struct wind
  ([origin : Integer]
   [scale : Float]))

(: set-wind! (planet-climate -> Void))
(define (set-wind! p)
  (let* ([tropical-equator (* 0.5 (planet-solar-equator p))]
         [edge-wind (make-flvector (edge-count p) 0.0)]
         [add (curry flvector-add! edge-wind)]
         [tile-edge-sign (let ([v (build-flvector (* 6 (tile-count p))
                                                  (λ ([n : Integer])
                                                    (let ([i (modulo n 6)]
                                                          [n (inexact->exact (floor (/ n 6)))])
                                                      (fl (edge-tile-sign p (tile-edge p n i) n)))))])
                           (λ ([n : Integer]
                               [i : Integer])
                             (flvector-ref v (+ i (* 6 n)))))])
    (for ([n (tile-count p)])
      (let* ([wind-vector (default-wind tropical-equator p n)]
             [tile-vector (tile-coordinates p n)]
             [negative-wind-vector (flvector3-negative wind-vector)]
             [tile-wind (build-flvector-ref (tile-edge-count n)
                                            (λ ([i : Integer])
                                              (let ([neighbour-vector (flvector3-normal (flvector3-rejection tile-vector
                                                                                                             (tile-coordinates p (tile-tile p n i))))])
                                                (* (flvector3-length (flvector3-projection neighbour-vector wind-vector))
                                                   (if (< (flvector3-distance-squared neighbour-vector wind-vector)
                                                          (flvector3-distance-squared neighbour-vector negative-wind-vector))
                                                       1.0
                                                       -1.0)))))])
        (for ([i (tile-edge-count n)])
          (let ([e (tile-edge p n i)])
            (add e (* 0.5
                      (tile-edge-sign n i)
                      (tile-wind i)))))))
    (for ([n (edge-count p)])
      ((edge-climate-data-air-flow-set! (planet-climate-edge p)) n (flvector-ref edge-wind n)))))

(: set-river-flow! (planet-climate -> Void))
(define (set-river-flow! planet)
  (: river-outflow (Integer -> Float))
  (define river-outflow (build-flvector-ref (tile-count planet)
                                            (λ ([n : Integer])
                                              (/ (* (tile-area planet n)
                                                    (tile-precipitation planet n))
                                                 (tile-edge-count n)))))
  (define river-flow-vec (make-flvector (corner-count planet) 0.0))
  (: river-flow (Integer -> Float))
  (define (river-flow n)
    (flvector-ref river-flow-vec n))
  (: set-corner-flow (Integer Float -> Void))
  (define (set-corner-flow corner flow)
    ((corner-climate-data-river-flow-set! (planet-climate-corner planet)) corner flow))
  (: visit-river (river -> Void))
  (define (visit-river r)
    (for ([n (river-sources r)])
      (visit-river n))
    (let ([n (river-location r)])
      (flvector-set! river-flow-vec
                     n
                     (+ (for/fold: ([sum : Float 0.0])
                                   ([i 3])
                          (+ sum (river-outflow (corner-tile planet n i))))
                        (for/fold: ([sum : Float 0.0])
                                   ([i (river-sources r)])
                          (+ sum (river-flow (river-location i)))))))
    (when-let* ([n (river-location r)]
                [dir (corner-river-direction planet n)])
               ((edge-climate-data-river-flow-set! (planet-climate-edge planet)) (corner-edge planet n dir) (river-flow n))))
  (for ([r (planet-rivers planet)])
    (visit-river r))
  (for ([c (corner-count planet)])
    (set-corner-flow c (for/fold ([flow 0.0])
                                 ([dir 3])
                         (if ((river-flows-to? planet c) (corner-corner planet c dir))
                             (+ flow (edge-river-flow planet (corner-edge planet c dir)))
                             flow)))))

(struct: climate-data
  ([tile-humidity : FlVector]
   [tile-precipitation : FlVector]))

(: make-climate-data (planet-climate -> climate-data))
(define (make-climate-data p)
  (climate-data
   (make-flvector (tile-count p) 0.0)
   (make-flvector (tile-count p) 0.0)))

(: generate-climate! (climate-parameters planet-climate planet-climate (String -> Any) -> Void))
(define (generate-climate! par prev p feedback)
  (let* ([acceptable-delta (climate-parameters-acceptable-delta par)]
         [precipitation-factor (climate-parameters-precipitation-factor par)]
         [humidity-half-life-days (climate-parameters-humidity-half-life-days par)]
         [humidity-half-life (* humidity-half-life-days seconds-per-day)]
         [humidity->precipitation-rate (/ (log 0.5) humidity-half-life)]
         [tile-water? (build-vector-ref (tile-count p) (curry tile-water? p))]
         [edge-length (build-flvector-ref (edge-count p) (curry edge-length p))])
    (define (climate-iterate!)
      (let* ([edge-wind (build-vector-ref
                         (edge-count p)
                         (λ ([n : Integer])
                           (let ([scale (* ((edge-climate-data-air-flow (planet-climate-edge p)) n)
                                           (edge-length n))])
                             (wind
                              (edge-tile p n (if (> 0.0 scale) 0 1))
                              (abs scale)))))]
             [wind-list/filter (λ ([f : (Integer Integer -> Boolean)])
                                 (build-vector-ref
                                  (tile-count p)
                                  (λ ([n : Integer])
                                    (map edge-wind
                                         (filter (curry f n)
                                                 (grid-tile-edge-list p n))))))]
             [incoming-winds (wind-list/filter (λ ([n : Integer]
                                                   [e : Integer])
                                                 (not (= n (wind-origin (edge-wind e))))))]
             [outgoing-winds (wind-list/filter (λ ([n : Integer]
                                                   [e : Integer])
                                                 (= n (wind-origin (edge-wind e)))))]
             [total-wind (λ ([ls : (Listof wind)])
                           (foldl + 0.0 (map wind-scale ls)))]
             [total-incoming-wind (build-flvector-ref
                                   (tile-count p)
                                   (λ ([n : Integer])
                                     (total-wind (incoming-winds n))))]
             [total-outgoing-wind (build-flvector-ref
                                   (tile-count p)
                                   (λ ([n : Integer])
                                     (total-wind (outgoing-winds n))))]
             [tile-precipitation-rate (build-flvector-ref (tile-count p)
                                                          (λ ([n : Integer])
                                                            (let* ([outgoing-wind (total-outgoing-wind n)]
                                                                   [traversal-time (if (zero? outgoing-wind)
                                                                                       seconds-per-day
                                                                                       (/ (tile-area p n)
                                                                                          outgoing-wind))])
                                                              (- 1.0 (exp (* traversal-time
                                                                             humidity->precipitation-rate))))))]
             [absolute-incoming-humidity (λ ([tile-humidity : (Integer -> Float)]
                                             [n : Integer])
                                           (for/fold: ([humidity : Float 0.0])
                                                      ([w (incoming-winds n)])
                                             (+ humidity
                                                (* (tile-humidity (wind-origin w))
                                                   (wind-scale w)))))])
        (: iterate! (climate-data climate-data Real -> climate-data))
        (define (iterate! to from delta)
          (feedback (string-append "delta " (number->string delta)))
          (if (< delta acceptable-delta)
              from
              (let* ([set-humidity! (λ ([n : Integer]
                                        [a : Float])
                                      (flvector-set! (climate-data-tile-humidity to) n a))]
                     [set-precipitation! (λ ([n : Integer]
                                             [a : Float])
                                           (flvector-set! (climate-data-tile-precipitation to) n a))]
                     [tile-humidity (λ ([n : Integer])
                                      (flvector-ref (climate-data-tile-humidity from) n))])
                (for ([n (tile-count p)])
                  (let* ([water? (tile-water? n)]
                         [outgoing-wind (total-outgoing-wind n)]
                         [incoming-wind (total-incoming-wind n)]
                         [saturation-humidity (saturation-humidity (tile-temperature p n))]
                         [incoming-humidity (absolute-incoming-humidity tile-humidity n)]
                         [preliminary-humidity (if water?
                                                   saturation-humidity
                                                   (if (zero? outgoing-wind)
                                                       (+ saturation-humidity incoming-humidity)
                                                       (/ incoming-humidity
                                                          outgoing-wind)))]
                         [traversal-precipitation (* preliminary-humidity
                                                     (tile-precipitation-rate n))]
                         [saturation-precipitation (max 0.0 (- preliminary-humidity saturation-humidity))]
                         [humidity->precipitation (if water?
                                                      0.0
                                                      (max traversal-precipitation
                                                           saturation-precipitation))]
                         [precipitation (/ (* precipitation-factor 0.2 humidity->precipitation (max (- incoming-wind outgoing-wind) outgoing-wind))
                                           (tile-area p n))]
                         [humidity (- preliminary-humidity humidity->precipitation)])
                    (set-humidity! n humidity)
                    (set-precipitation! n precipitation)))
                (iterate! from to (apply max (map (λ ([n : Integer])
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
            (flvector-set! (climate-data-tile-humidity from) n (if (tile-water? n)
                                                                   (saturation-humidity (tile-temperature p n))
                                                                   0.0)))
          (let ([climate-values (iterate! (make-climate-data p)
                                          from
                                          1.0)])
            (let ([set-humidity (tile-climate-data-humidity-set! (planet-climate-tile p))]
                  [humidity-data (climate-data-tile-humidity climate-values)]
                  [set-precipitation (tile-climate-data-precipitation-set! (planet-climate-tile p))]
                  [precipitation-data (climate-data-tile-precipitation climate-values)]
                  [set-leaf-area-index (tile-climate-data-leaf-area-index-set! (planet-climate-tile p))])
              (for ([n (tile-count p)])
                (set-humidity n (flvector-ref humidity-data n))
                (set-precipitation n (flvector-ref precipitation-data n))
                (set-leaf-area-index n (supported-leaf-area-index (tile-insolation p n)
                                                                  (tile-temperature p n)
                                                                  (tile-precipitation p n)))))))))
    (set-wind! p)
    (climate-iterate!)
    (set-river-flow! p)
    (void)))

(: initial-values (climate-parameters planet-climate -> planet-climate))
(define (initial-values par prev)
  (let* ([tile-count (tile-count prev)]
         [corner-count (corner-count prev)]
         [edge-count (edge-count prev)]
         [axial-tilt (climate-parameters-axial-tilt par)]
         [tile-latitude (curry tile-latitude prev)]
         [seasons-per-cycle (climate-parameters-seasons-per-cycle par)]
         [season (modulo (+ 1 (planet-climate-season prev))
                         (climate-parameters-seasons-per-cycle par))]
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
                            (default-temperature prev (vector-accessor-get tile-sunlight)))]
         [tile-snow-cover (build-flvector-accessor
                           tile-count
                           (default-snow-cover prev (vector-accessor-get tile-temperature)))])
    (struct-copy planet-climate prev
                 [season season]
                 [tile (tile-climate-data/accessors
                        #:sunlight tile-sunlight
                        #:temperature tile-temperature
                        #:humidity (zeros tile-count)
                        #:precipitation (zeros tile-count)
                        #:snow tile-snow-cover
                        #:leaf-area-index (zeros tile-count))]
                 [corner (corner-climate-data/accessors
                          #:river-flow (zeros corner-count))]
                 [edge (edge-climate-data/accessors
                        #:river-flow (zeros edge-count)
                        #:air-flow (zeros edge-count))])))
