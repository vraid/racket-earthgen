#lang typed/racket

(require math/flonum
         "flvector3.rkt")

(provide matrix3
         matrix3-identity
         matrix3+
         matrix3-
         matrix3*
         matrix3-vector3*)

(define-type matrix3 FlVector)

(: matrix3-identity (-> matrix3))
(define (matrix3-identity)
  (vector->flvector
   (vector 1 0 0
           0 1 0
           0 0 1)))

(: matrix3-zero (-> matrix3))
(define (matrix3-zero)
  (vector->flvector
   (make-vector 9 0)))

(: matrix3+ (matrix3 * -> matrix3))
(define (matrix3+ . ms)
  (foldl flvector+ (matrix3-zero) ms))

(: matrix3- (matrix3 matrix3 * -> matrix3))
(define (matrix3- a . ms)
  (if (empty? ms)
      (flvector- a)
      (foldl (lambda: ([a : matrix3]
                       [b : matrix3])
               (flvector- b a))
             a ms)))

(: matrix3-element (matrix3 Integer Integer -> Flonum))
(define (matrix3-element m i j)
  (flvector-ref m (+ i (* 3 j))))

(: matrix3-row (matrix3 Integer -> flvector3))
(define (matrix3-row m r)
  (let ([el (lambda: ([c : Integer])
              (matrix3-element m r c))])
    (flvector (el 0) (el 1) (el 2))))

(: matrix3-column (matrix3 Integer -> flvector3))
(define (matrix3-column m c)
  (let ([el (lambda: ([r : Integer])
              (matrix3-element m r c))])
    (flvector (el 0) (el 1) (el 2))))

(: element-sum (FlVector -> Flonum))
(define (element-sum v)
  (foldl + 0.0 (flvector->list v)))

(: matrix3-single* (matrix3 matrix3 -> matrix3))
(define (matrix3-single* a b)
  (let ([m (lambda: ([r : Integer]
                     [c : Integer])
             (element-sum
              (flvector*
               (matrix3-row a r)
               (matrix3-column b c))))])
    (flvector (m 0 0) (m 0 1) (m 0 2)
              (m 1 0) (m 1 1) (m 1 2)
              (m 2 0) (m 2 1) (m 2 2))))

(: matrix3* (matrix3 * -> matrix3))
(define (matrix3* . ms)
  (foldl matrix3-single* (matrix3-identity) ms))

(: matrix3-vector3* (matrix3 flvector3 -> flvector3))
(define (matrix3-vector3* a v)
  (let ([m (lambda: ([r : Integer])
             (element-sum
              (flvector*
               (matrix3-row a r)
               v)))])
    (flvector (m 0) (m 1) (m 2))))
