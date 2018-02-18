#lang typed/racket

(provide (all-defined-out))

(require racket/flonum)

(define sqrt2 (sqrt 2.0))

(: relative->rectangular (Integer Integer -> (FlVector -> (Vectorof Integer))))
(define (relative->rectangular width height)
  (lambda: ([coord : FlVector])
    (let ([lon (flvector-ref coord 0)]
          [lat (flvector-ref coord 1)])
      (vector (max 0 (modulo (round  (inexact->exact (* width (/ (+ 1.0 lon) 2.0))))
                             width))
              (max 0 (modulo (round (inexact->exact (* height (+ 0.5 lat))))
                             height))))))

(: equirectangular-projection (Float Float -> FlVector))
(define (equirectangular-projection longitude latitude)
  (flvector (/ longitude pi) (/ (- latitude) pi)))

(: orthographic->spherical (FlVector -> FlVector))
(define (orthographic->spherical coord)
  (let* ([x (flvector-ref coord 0)]
         [y (flvector-ref coord 1)]
         [z (flsqrt (- 1.0 (flexpt x 2.0) (flexpt y 2.0)))])
    (flvector x y z)))

(: longitude (FlVector -> Float))
(define (longitude coord)
  (atan (flvector-ref coord 1)
        (flvector-ref coord 0)))

(: spherical->hammer (FlVector -> (FlVector -> FlVector)))
(define (spherical->hammer center)
  (let* ([center-sign (if (< (flvector-ref center 0) 0.0)
                          (sgn (longitude center))
                          0.0)])
    (λ (coord)
      (let* ([latitude (asin (flvector-ref coord 2))]
             [lon (longitude coord)]
             [longitude (+ lon
                           (if (= (sgn lon) center-sign)
                               0.0
                               (* 2.0 pi center-sign)))]
             [cos-lat (cos latitude)]
             [scale (/ 1.0 (flsqrt (+ 1.0 (* cos-lat (cos (* 0.5 longitude))))))]
             [x (* 2.0 sqrt2 (cos latitude) (sin (* 0.5 longitude)) scale)]
             [y (* sqrt2 (sin latitude) scale)])
        (flvector x y 0.0)))))

(: hammer->spherical (FlVector -> (Option FlVector)))
(define (hammer->spherical coord)
  (let* ([sq (λ ([k : Float]
                 [a : Float])
              (flexpt (* k a) 2.0))]
         [x (flvector-ref coord 0)]
         [y (flvector-ref coord 1)]
         [xsq (sq 0.25 x)]
         [ysq (sq 0.5 y)]
         [xysum (+ xsq ysq)])
    (if (< 1 xysum)
        #f
        (let* ([z (flsqrt (- 1.0 xsq ysq))]
               [latitude (flasin (* z y))]
               [longitude (* 2.0 (atan (/ (* z x) (* 2.0 (- (* 2.0 z z) 1.0)))))]
               [cos-lat (flcos latitude)]
               [x (* cos-lat (flcos longitude))]
               [y (* cos-lat (flsin longitude))]
               [z (flsin latitude)])
          (flvector x y z)))))
