#lang typed/racket

(provide (all-defined-out))

(require vraid/flow
         vraid/math
         vraid/util
         math/flonum
         "../grid.rkt"
         "../geometry.rkt"
         "../terrain.rkt"
         "../water.rkt"
         "../climate.rkt")

(: default-temperature (planet-climate Integer -> Float))
(define (default-temperature p n)
  (-
   (+ 220.0
      (* (/ 110.0 solar-constant)
         (if (tile-land? p n)
             (tile-sunlight p n)
             (sunlight 0.0 (tile-latitude p n)))))
   (max 0.0
        (temperature-lapse (tile-elevation p n)))))

(: default-snow-cover (planet-climate Integer -> Float))
(define (default-snow-cover p n)
  (let ([temperature (tile-temperature p n)])
    (if (tile-water? p n)
        0.0
        (if (below-freezing-temperature? temperature)
            1.0
            0.0))))

(: default-pressure-gradient-force (Float Float -> FlVector))
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

(: default-wind (Float planet-climate Integer -> FlVector))
(define (default-wind tropical-equator p n)
  (prevailing-wind p
                   (tile-coordinates p n)
                   (default-pressure-gradient-force tropical-equator (tile-latitude p n))
                   (tile-surface-friction p n)))

(struct wind
  ([origin : Integer]
   [scale : Float]))

(: set-wind! (planet-climate -> Void))
(define (set-wind! p)
  (let* ([tropical-equator (* 0.5 (planet-solar-equator p))]
         [edge-wind (make-flvector (edge-count p) 0.0)]
         [add (lambda ([n : Integer]
                       [a : Float])
                (flvector-set! edge-wind n (+ a (flvector-ref edge-wind n))))]
         [tile-edge-sign (let ([v (build-vector (* 6 (tile-count p))
                                                (lambda ([n : Integer])
                                                  (let ([i (modulo n 6)]
                                                        [n (inexact->exact (floor (/ n 6)))])
                                                    (edge-tile-sign p (tile-edge p n i) n))))])
                           (lambda ([p : planet-climate]
                                    [n : Integer]
                                    [i : Integer])
                             (vector-ref v (+ i (* 6 n)))))])
    (for ([n (tile-count p)])
      (let* ([wind-vector (default-wind tropical-equator p n)]
             [tile-vector (tile-coordinates p n)]
             [negative-wind-vector (flvector3-negative wind-vector)]
             [tile-wind (build-flvector-ref (tile-edge-count n)
                                            (lambda (i)
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
                      (tile-edge-sign p n i)
                      (tile-wind i)))))))
    (for ([n (edge-count p)])
      ((edge-climate-data-air-flow-set! (planet-climate-edge p)) n (flvector-ref edge-wind n)))))

(: set-river-flow! (planet-climate -> Void))
(define (set-river-flow! planet)
  (: river-outflow (Integer -> Float))
  (define river-outflow (build-flvector-ref (tile-count planet)
                                            (lambda ([n : Integer])
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
