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
