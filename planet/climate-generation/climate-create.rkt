#lang typed/racket

(provide static-climate
         singular-climate
         climate-next
         climate-parameters/kw
         default-climate-parameters
         default-wind)

(require math/flonum
         vraid/util
         "../grid-base.rkt"
         "../geometry.rkt"
         "../water.rkt"
         "../climate.rkt"
         "climate-create-base.rkt"
         "../terrain-generation/planet-create.rkt"
         "../terrain-generation/river-generation.rkt")

(: new-flvector-accessor (Integer -> (vector-accessor Float)))
(define (new-flvector-accessor count)
  (make-flvector-accessor
   (make-flvector count 0.0)))

(: make-tile-climate-data (Integer -> tile-climate-data))
(define (make-tile-climate-data tile-count)
  (let ([new-flvector (thunk (new-flvector-accessor tile-count))])
    (tile-climate-data/accessors
     #:snow (new-flvector)
     #:sunlight (new-flvector)
     #:temperature (new-flvector)
     #:humidity (new-flvector)
     #:precipitation (new-flvector)
     #:leaf-area-index (new-flvector))))

(: make-corner-climate-data (Integer -> corner-climate-data))
(define (make-corner-climate-data corner-count)
  (let ([new-flvector (thunk (new-flvector-accessor corner-count))])
    (corner-climate-data/accessors
     #:river-flow (new-flvector))))

(: make-edge-climate-data (Integer -> edge-climate-data))
(define (make-edge-climate-data edge-count)
  (let ([new-flvector (thunk (new-flvector-accessor edge-count))])
    (edge-climate-data/accessors
     #:river-flow (new-flvector)
     #:air-flow (new-flvector))))

(: singular-climate (climate-parameters (String -> Any) -> (planet-water -> planet-climate)))
(define ((singular-climate param feedback) planet)
  (let* ([planet/rivers (planet/rivers planet)]
         [initial ((climate/closed-season param planet/rivers feedback) 0)]
         [p ((climate/closed-season param planet/rivers feedback) 0)])
    (generate-climate! param initial p feedback)
    p))

(: static-climate (climate-parameters planet-water (String -> Any) -> ((Option planet-climate) -> planet-climate)))
(define (static-climate param planet feedback)
  (let* ([planet/rivers (planet/rivers planet)]
         [season-count (climate-parameters-seasons-per-cycle param)]
         [initial ((climate/closed-season param planet/rivers feedback) 0)]
         [v (build-vector season-count
                          (lambda ([n : Integer])
                            (delay (let ([p ((climate/closed-season param planet/rivers feedback) n)])
                                     (generate-climate! param initial p feedback)
                                     p))))])
    (lambda ([planet : (Option planet-climate)])
      (let ([season (if planet
                        (modulo (+ 1 (planet-climate-season planet))
                                season-count)
                        0)])
        (force (vector-ref v season))))))

(: climate/closed-season (climate-parameters planet-water (String -> Any) -> (Integer -> planet-climate)))
(define ((climate/closed-season par planet feedback) season)
  (let ([p (planet-climate/kw
            #:planet-water planet
            #:parameters par
            #:season season
            #:tile (make-tile-climate-data (tile-count planet))
            #:corner (make-corner-climate-data (corner-count planet))
            #:edge (make-edge-climate-data (edge-count planet)))])
    (let ([init-tile-array (tile-init p)])
      (init-tile-array (tile-climate-data-sunlight-set! (planet-climate-tile p))
                       (lambda ([n : Integer])
                         (sunlight
                          (planet-solar-equator p)
                          (tile-latitude p n))))
      (init-tile-array (tile-climate-data-temperature-set! (planet-climate-tile p))
                       (curry default-temperature p))
      (init-tile-array (tile-climate-data-snow-set! (planet-climate-tile p))
                       (curry default-snow-cover p)))
    p))

(: climate-next (climate-parameters planet-climate (String -> Any) -> planet-climate))
(define (climate-next par prev feedback)
  (let ([p (initial-values par prev)])
    (generate-climate! par prev p feedback)
    p))

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
         [tile-water? (let ([v (build-vector (tile-count p)
                                             (lambda ([n : Integer])
                                               (tile-water? p n)))])
                        (lambda ([p : planet-climate]
                                 [n : Integer])
                          (vector-ref v n)))]
         [tile-land? (lambda ([p : planet-climate]
                              [n : Integer])
                       (not (tile-water? p n)))]
         [edge-length (build-flvector-ref (edge-count p) (curry edge-length p))]
         [edge-tile-distances (build-flvector (edge-count p) (curry edge-tile-distance p))]
         [tile-tile-distance (lambda ([p : planet-climate]
                                      [n : Integer]
                                      [i : Integer])
                               (flvector-ref edge-tile-distances (tile-edge p n i)))])
    (define (climate-iterate!)
      (let* ([edge-wind (build-vector-ref
                         (edge-count p)
                         (lambda ([n : Integer])
                           (let ([scale (* ((edge-climate-data-air-flow (planet-climate-edge p)) n)
                                           (edge-length n))])
                             (wind
                              (edge-tile p n (if (> 0.0 scale) 0 1))
                              (abs scale)))))]
             [wind-list/filter (lambda ([f : (Integer Integer -> Boolean)])
                                 (build-vector-ref
                                  (tile-count p)
                                  (lambda ([n : Integer])
                                    (map edge-wind
                                         (filter (curry f n)
                                                 (grid-tile-edge-list p n))))))]
             [incoming-winds (wind-list/filter (lambda ([n : Integer]
                                                        [e : Integer])
                                                 (not (= n (wind-origin (edge-wind e))))))]
             [outgoing-winds (wind-list/filter (lambda ([n : Integer]
                                                        [e : Integer])
                                                 (= n (wind-origin (edge-wind e)))))]
             [total-wind (lambda ([ls : (Listof wind)])
                           (foldl + 0.0 (map wind-scale ls)))]
             [total-incoming-wind (build-flvector-ref
                                   (tile-count p)
                                   (lambda ([n : Integer])
                                     (total-wind (incoming-winds n))))]
             [total-outgoing-wind (build-flvector-ref
                                   (tile-count p)
                                   (lambda ([n : Integer])
                                     (total-wind (outgoing-winds n))))]
             [tile-precipitation-rate (build-flvector-ref (tile-count p)
                                                          (lambda ([n : Integer])
                                                            (let* ([outgoing-wind (total-outgoing-wind n)]
                                                                   [traversal-time (if (zero? outgoing-wind)
                                                                                       seconds-per-day
                                                                                       (/ (tile-area p n)
                                                                                          outgoing-wind))])
                                                              (- 1.0 (exp (* traversal-time
                                                                             humidity->precipitation-rate))))))]
             [absolute-incoming-humidity (lambda ([tile-humidity : (Integer -> Float)]
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
              (let* ([set-humidity! (lambda ([n : Integer]
                                             [a : Float])
                                      (flvector-set! (climate-data-tile-humidity to) n a))]
                     [set-precipitation! (lambda ([n : Integer]
                                                  [a : Float])
                                           (flvector-set! (climate-data-tile-precipitation to) n a))]
                     [tile-humidity (lambda ([n : Integer])
                                      (flvector-ref (climate-data-tile-humidity from) n))])
                (for ([n (tile-count p)])
                  (let* ([water? (tile-water? p n)]
                         [outgoing-wind (total-outgoing-wind n)]
                         [incoming-wind (total-incoming-wind n)]
                         [max-wind (max outgoing-wind incoming-wind)]
                         [traversal-time (if (zero? max-wind)
                                             seconds-per-day
                                             (/ (tile-area p n)
                                                max-wind))]
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
                (iterate! from to (apply max (map (lambda ([n : Integer])
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
  (let* ([p (struct-copy planet-climate prev
                         [season (modulo (+ 1 (planet-climate-season prev))
                                         (climate-parameters-seasons-per-cycle par))]
                         [tile (make-tile-climate-data (tile-count prev))]
                         [edge (make-edge-climate-data (edge-count prev))])]
         [init-tile-array (init-array (tile-count p))]
         [tile (planet-climate-tile p)])
    (init-tile-array (tile-climate-data-sunlight-set! tile)
                     (lambda ([n : Integer])
                       (sunlight
                        (planet-solar-equator p)
                        (tile-latitude p n))))
    (init-tile-array (tile-climate-data-temperature-set! tile)
                     (curry default-temperature p))
    (init-tile-array (tile-climate-data-snow-set! tile)
                     (curry default-snow-cover p))
    p))
