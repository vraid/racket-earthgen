#lang typed/racket

(require math/flonum
         racket/vector)

(provide flvector3
         flvector3-zero
         flvector3-scale
         flvector3-length
         flvector3-length-squared
         flvector3-distance
         flvector3-distance-squared
         flvector3-normal
         flvector3+
         flvector3-
         flvector3-dot-product
         flvector3-cross-product
         flvector3-map-mult)

(define-type flvector3 FlVector)

(: flvector3-zero (-> flvector3))
(define (flvector3-zero)
  (flvector 0.0 0.0 0.0))

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
  (flvector3-length-squared (flvector3- v u)))

(: flvector3-distance (flvector3 flvector3 -> Flonum))
(define (flvector3-distance v u)
  (flvector3-length (flvector3- v u)))

(: flvector3-scale (flvector3 Flonum -> flvector3))
(define (flvector3-scale v a)
  (flvector-scale v a))

(: flvector3-normal (flvector3 -> flvector3))
(define (flvector3-normal a)
  (if (zero? (flvector3-length a))
      a
      (flvector3-scale a (/ (flvector3-length a)))))

(: flvector3+ (flvector3 * -> flvector3))
(define (flvector3+ . vecs)
    (foldl flvector+
           (flvector3-zero) vecs))

(: flvector3- (flvector3 flvector3 * -> flvector3))
(define (flvector3- v . vecs)
  (if (empty? vecs) 
      (flvector- v)
      (foldl (lambda: ([b : flvector3]
                       [a : flvector3])
               (flvector- a b))
             v vecs)))

(: mult (Flonum Flonum * -> Flonum))
(define (mult a . n)
  (foldl * a n))

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

(: remap (flvector3 (Vectorof Integer) -> flvector3))
(define (remap v m)
  (let ([elm (lambda: ([v : flvector3]
                       [m : (Vectorof Integer)]
                       [i : Integer])
               (flvector-ref v (vector-ref m i)))])
    (flvector (elm v m 0)
              (elm v m 1) 
              (elm v m 2))))

(: col (flvector3 (Vectorof Integer) flvector3 (Vectorof Integer) -> flvector3))
(define (col v m u n)
  (flvector-map mult
                (remap v m)
                (remap u n)))

(: flvector3-cross-product (flvector3 flvector3 -> flvector3))
(define (flvector3-cross-product v u)
  (let* ([m (vector 1 2 0)]
         [n (vector 2 0 1)])
    (flvector3- (col v m u n) 
              (col v n u m))))
