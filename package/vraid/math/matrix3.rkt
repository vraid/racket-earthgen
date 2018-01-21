#lang typed/racket

(require math/flonum
         "flvector3.rkt")

(provide matrix3-identity
         matrix3-sum
         matrix3-subtract
         matrix3-product
         matrix3-vector3-product)

(: matrix3-identity (-> FlVector))
(define (matrix3-identity)
  (vector->flvector
   (vector 1 0 0
           0 1 0
           0 0 1)))

(: matrix3-zero (-> FlVector))
(define (matrix3-zero)
  (vector->flvector
   (make-vector 9 0)))

(: matrix3-sum (FlVector * -> FlVector))
(define (matrix3-sum . ms)
  (foldl flvector+ (matrix3-zero) ms))

(: matrix3-subtract (FlVector FlVector * -> FlVector))
(define (matrix3-subtract a . ms)
  (if (empty? ms)
      (flvector- a)
      (foldl (λ ([a : FlVector]
                 [b : FlVector])
               (flvector- b a))
             a ms)))

(: matrix3-element (FlVector Integer Integer -> Float))
(define (matrix3-element m i j)
  (flvector-ref m (+ i (* 3 j))))

(: matrix3-row (FlVector Integer -> FlVector))
(define (matrix3-row m r)
  (let ([el (λ ([c : Integer])
              (matrix3-element m r c))])
    (apply flvector (map el (range 3)))))

(: matrix3-column (FlVector Integer -> FlVector))
(define (matrix3-column m c)
  (let ([el (λ ([r : Integer])
              (matrix3-element m r c))])
    (apply flvector (map el (range 3)))))

(: element-sum (FlVector -> Float))
(define (element-sum v)
  (foldl + 0.0 (flvector->list v)))

(: matrix3-single-product (FlVector FlVector -> FlVector))
(define (matrix3-single-product a b)
  (let ([m (λ ([r : Integer]
               [c : Integer])
             (element-sum
              (flvector*
               (matrix3-row a r)
               (matrix3-column b c))))])
    (flvector (m 0 0) (m 0 1) (m 0 2)
              (m 1 0) (m 1 1) (m 1 2)
              (m 2 0) (m 2 1) (m 2 2))))

(: matrix3-product (FlVector * -> FlVector))
(define (matrix3-product . ms)
  (foldl matrix3-single-product (matrix3-identity) ms))

(: matrix3-vector3-product (FlVector FlVector -> FlVector))
(define (matrix3-vector3-product a v)
  (let ([m (λ ([r : Integer])
             (element-sum
              (flvector*
               (matrix3-row a r)
               v)))])
    (apply flvector (map m (range 3)))))
