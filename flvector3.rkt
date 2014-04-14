#lang typed/racket

(provide (all-defined-out))

(require "flvector3-local.rkt"
         "math.rkt"
         math/flonum)

(define-type flvector3 FlVector)

(: flvector3-zero (-> flvector3))
(define (flvector3-zero)
  (flvector 0.0 0.0 0.0))

(: flvector3-zero? (flvector3 -> Boolean))
(define (flvector3-zero? v)
  (zero? (flvector3-length-squared v)))

(: flvector3-negative (flvector3 -> flvector3))
(define (flvector3-negative v)
  (flvector3-scale -1.0 v))

(: flvector3-length-squared (flvector3 -> Flonum))
(define (flvector3-length-squared v)
  (flvector-sum
   (flvector-sqr v)))

(: flvector3-length (flvector3 -> Flonum))
(define (flvector3-length v)
  (flsqrt 
   (flvector3-length-squared v)))

(: flvector3-distance-squared (flvector3 flvector3 -> Flonum))
(define (flvector3-distance-squared v u)
  (flvector3-length-squared (flvector3-subtract v u)))

(: flvector3-distance (flvector3 flvector3 -> Flonum))
(define (flvector3-distance v u)
  (flvector3-length (flvector3-subtract v u)))

(: flvector3-scale (Flonum flvector3 -> flvector3))
(define (flvector3-scale a v)
  (flvector-scale v a))

(: flvector3-scale-to (Flonum flvector3 -> flvector3))
(define (flvector3-scale-to a v)
  (flvector3-scale (divide (flvector3-length v)
                           a)
                   v))

(: flvector3-normal (flvector3 -> flvector3))
(define (flvector3-normal a)
  (if (zero? (flvector3-length-squared a))
      a
      (flvector3-scale (/ (flvector3-length a)) a)))

(: flvector3-sum (flvector3 * -> flvector3))
(define (flvector3-sum . vecs)
  (foldl flvector+
         (flvector3-zero) vecs))

(: flvector3-subtract (flvector3 flvector3 -> flvector3))
(define (flvector3-subtract a b)
  (flvector- b a))

(: flvector3-map-mult (flvector3 * -> flvector3))
(define (flvector3-map-mult . vecs)
  (foldl (lambda: ([v : flvector3]
                   [u : flvector3])
           (flvector-map mult v u))
         (flvector 1.0 1.0 1.0) vecs))

(: flvector3-dot-product (flvector3 flvector3 -> Flonum))
(define (flvector3-dot-product v u)
  (flvector-sum
   (flvector-map mult v u)))

(: flvector3-cross-product (flvector3 flvector3 -> flvector3))
(define (flvector3-cross-product v u)
  (let* ([m (vector 1 2 0)]
         [n (vector 2 0 1)])
    (flvector3-subtract (col v m u n)
                        (col v n u m))))

(: flvector3-angle (flvector3 flvector3 -> Flonum))
(define (flvector3-angle a b)
  (flacos (fl/ (flvector3-dot-product a b)
               (flsqrt (* (flvector3-length-squared a)
                          (flvector3-length-squared b))))))

(: flvector3-projection (flvector3 flvector3 -> flvector3))
(define (flvector3-projection target v)
  (let ([n (flvector3-normal target)])
    (flvector3-scale (flvector3-dot-product n v) n)))

(: flvector3-rejection (flvector3 flvector3 -> flvector3))
(define (flvector3-rejection rejector v)
  (flvector3-subtract (flvector3-projection rejector v) v))
