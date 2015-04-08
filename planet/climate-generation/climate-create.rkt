#lang typed/racket

(provide static-climate
         climate-next
         climate-parameters/kw
         default-climate-parameters
         default-wind)

(require math/flonum
         vraid/types
         vraid/flow
         vraid/math
         vraid/typed-array
         "../grid.rkt"
         "../geometry.rkt"
         "../terrain.rkt"
         "../climate.rkt"
         "climate-create-base.rkt"
         "../terrain-generation/river-generation.rkt")

(: static-climate (climate-parameters planet-terrain -> ((maybe planet-climate) -> planet-climate)))
(define (static-climate param terrain)
  (let* ([terrain/rivers (generate-rivers terrain)]
         [season-count (climate-parameters-seasons-per-cycle param)]
         [initial ((climate/closed-season param terrain/rivers) 0)]
         [v (build-vector season-count
                          (lambda ([n : integer])
                            (delay (let ([p ((climate/closed-season param terrain/rivers) n)])
                                     (generate-climate! param initial p)
                                     p))))])
    (lambda ([planet : (maybe planet-climate)])
      (let ([season (if planet
                        (modulo (+ 1 (planet-climate-season planet))
                                season-count)
                        0)])
        (force (vector-ref v season))))))

(: climate/closed-season (climate-parameters planet-terrain -> (integer -> planet-climate)))
(define ((climate/closed-season par terrain) season)
  (let ([p (planet-climate/kw
            #:planet-terrain terrain
            #:parameters par
            #:season season
            #:tile (make-tile-climate-data (tile-count terrain))
            #:edge (make-edge-climate-data (edge-count terrain)))])
    (let ([init-tile-array (init-array (tile-count p))])
      (init-tile-array (tile-climate-data-sunlight-set! (planet-climate-tile p))
                       (lambda ([n : integer])
                         (sunlight
                          (planet-solar-equator p)
                          (tile-latitude p n))))
      (init-tile-array (tile-climate-data-temperature-set! (planet-climate-tile p))
                       (curry default-temperature p))
      (init-tile-array (tile-climate-data-snow-cover-set! (planet-climate-tile p))
                       (curry default-snow-cover p)))
    p))

(: climate-next (climate-parameters planet-climate -> planet-climate))
(define (climate-next par prev)
  (let ([p (initial-values par prev)])
    (generate-climate! par prev p)
    p))

(struct: climate-data
  ([tile-humidity : FlVector]))

(: make-climate-data (planet-climate -> climate-data))
(define (make-climate-data p)
  (climate-data
   (make-flvector (tile-count p) 0.0)))

(: generate-climate! (climate-parameters planet-climate planet-climate -> Void))
(define (generate-climate! par prev p)
  (let* ([tile-water? (let ([v (build-vector (tile-count p)
                                             (lambda ([n : integer])
                                               (tile-water? p n)))])
                        (lambda ([p : planet-climate]
                                 [n : Integer])
                          (vector-ref v n)))]
         [tile-land? (lambda ([p : planet-climate]
                              [n : Integer])
                       (not (tile-water? p n)))]
         [edge-lengths (build-flvector (edge-count p) (curry edge-length p))]
         [edge-length (lambda ([n : integer])
                        (flvector-ref edge-lengths n))]
         [edge-tile-distances (build-flvector (edge-count p) (curry edge-tile-distance p))]
         [tile-tile-distance (lambda ([p : planet-climate]
                                      [n : integer]
                                      [i : integer])
                               (flvector-ref edge-tile-distances (tile-edge p n i)))])
    (define (climate-iterate!)
      (let* ([edge-wind (let ([winds (build-vector
                                      (edge-count p)
                                      (lambda ([n : integer])
                                        (let ([scale (* ((edge-climate-data-air-flow (planet-climate-edge p)) n)
                                                        (edge-length n))])
                                          (wind
                                           (edge-tile p n (if (> 0.0 scale) 0 1))
                                           (abs scale)))))])
                          (lambda ([n : integer])
                            (vector-ref winds n)))]
             [wind-list/filter (lambda ([f : (integer integer -> Boolean)])
                                 (let ([v (build-vector
                                           (tile-count p)
                                           (lambda ([n : integer])
                                             (map edge-wind
                                                  (filter (curry f n)
                                                          (grid-tile-edge-list p n)))))])
                                   (lambda ([n : integer])
                                     (vector-ref v n))))]
             [incoming-winds (wind-list/filter (lambda ([n : integer]
                                                        [e : integer])
                                                 (not (= n (wind-origin (edge-wind e))))))]
             [outgoing-winds (wind-list/filter (lambda ([n : integer]
                                                        [e : integer])
                                                 (= n (wind-origin (edge-wind e)))))]
             [total-wind (lambda ([ls : (Listof wind)])
                           (foldl + 0.0 (map wind-scale ls)))]
             [total-incoming-wind (lambda ([n : integer])
                                    (total-wind (incoming-winds n)))]
             [total-outgoing-wind (lambda ([n : integer])
                                    (total-wind (outgoing-winds n)))]
             [absolute-incoming-humidity (lambda ([tile-humidity : (integer -> flonum)]
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
              (let* ([set-tile-humidity! (lambda ([n : integer]
                                                  [a : Flonum])
                                           (flvector-set! (climate-data-tile-humidity to) n a))]
                     [tile-humidity (lambda ([n : integer])
                                      (flvector-ref (climate-data-tile-humidity from) n))])
                (for ([n (tile-count p)])
                  (set-tile-humidity! n (let ([saturation-humidity (saturation-humidity (tile-temperature p n))])
                                          (if (tile-water? p n)
                                              saturation-humidity
                                              (min saturation-humidity
                                                   (let ([outgoing (total-outgoing-wind n)])
                                                     (if (zero? outgoing)
                                                         saturation-humidity
                                                         (/ (absolute-incoming-humidity tile-humidity n)
                                                            outgoing))))))))
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
            (for ([n (tile-count p)])
              ((tile-climate-data-humidity-set! (planet-climate-tile p)) n
                                                                         (flvector-ref (climate-data-tile-humidity climate-values) n)))
            (for ([n (tile-count p)])
              ((tile-climate-data-precipitation-set! (planet-climate-tile p)) n
                                                                              (let ([outgoing (* (tile-humidity p n)
                                                                                                 (total-outgoing-wind n))]
                                                                                    [incoming (absolute-incoming-humidity (curry tile-humidity p) n)])
                                                                                (max 0.0 (/ (* 200.0 (- incoming outgoing))
                                                                                            (tile-area p n))))))))))
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
                     (lambda ([n : integer])
                       (sunlight
                        (planet-solar-equator p)
                        (tile-latitude p n))))
    (init-tile-array (tile-climate-data-temperature-set! tile)
                     (curry default-temperature p))
    (init-tile-array (tile-climate-data-snow-cover-set! tile)
                     (curry default-snow-cover p))
    p))
