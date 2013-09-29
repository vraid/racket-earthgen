#lang racket
(require math/flonum
         racket/vector)

(provide vector3
         vector3-zero
         vector3-scale
         vector3-length
         vector3-length-squared
         vector3-distance
         vector3-distance-squared
         vector3-normal
         vector3+
         vector3-
         vector3-dot-product
         vector3-cross-product
         vector3-map-mult)

(define (vector3 a b c)
  (flvector a b c))

(define vector3-zero
  (flvector 0.0 0.0 0.0))

(define (vector3-length-squared v)
  (flvector-sum
   (flvector-sqr v)))

(define (vector3-length v)
  (flsqrt 
   (vector3-length-squared v)))

(define (vector3-distance-squared v u)
  (vector3-length-squared (vector3- v u)))

(define (vector3-distance v u)
  (vector3-length (vector3- v u)))

(define vector3-scale
  flvector-scale)

(define (vector3-normal a)
  (if (zero? (vector3-length a))
      a
      (vector3-scale a (/ (vector3-length a)))))

(define (vector3+ . vecs)
    (foldl flvector+
           vector3-zero vecs))

(define (vector3- v . vecs)
  (if (empty? vecs) 
      (flvector- v)
      (foldl (lambda (b a) (flvector- a b))
             v vecs)))

(define (mult a . n)
  (foldl fl* a n))

(define (vector3-map-mult . vecs)
    (foldl (lambda (v u) (flvector-map mult v u)) 
           (vector3 1.0 1.0 1.0) vecs))

(define (vector3-dot-product v u)
  (flvector-sum
   (flvector-map mult v u)))

(define (remap v m)
  (let ([elm (lambda (v m i) (flvector-ref v (vector-ref m i)))])
    (vector3 (elm v m 0)
             (elm v m 1) 
             (elm v m 2))))

(define (col v m u n)
  (flvector-map mult
                (remap v m)
                (remap u n)))

(define (vector3-cross-product v u)
  (let* ([m (vector 1 2 0)]
         [n (vector 2 0 1)])
    (vector3- (col v m u n) 
              (col v n u m))))
