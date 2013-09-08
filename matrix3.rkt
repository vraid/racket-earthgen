#lang racket

(require math/flonum
         "vector3.rkt")

(provide matrix3-identity
         matrix3+
         matrix3-
         matrix3*
         matrix3-vector3*)

(define matrix3-identity
  (vector->flvector
   (vector 1 0 0
           0 1 0
           0 0 1)))

(define matrix3-zero
  (vector->flvector
   (make-vector 9 0)))

(define (matrix3+ . ms)
  (foldl flvector+ matrix3-zero ms))

(define (matrix3- a . ms)
  (if (empty? ms)
      (flvector- a)
      (foldl (lambda (a b) (flvector- b a)) a ms)))

(define (matrix3-element m i j)
  (flvector-ref m (+ i (* 3 j))))

(define (matrix3-row m r)
  (let ([el (lambda (c) (matrix3-element m r c))])
    (flvector (el 0) (el 1) (el 2))))

(define (matrix3-column m c)
  (let ([el (lambda (r) (matrix3-element m r c))])
    (flvector (el 0) (el 1) (el 2))))

(define (element-sum v)
  (let ([add (lambda (a b) (fl+ a b))])
    (foldl add 0.0 (flvector->list v))))

(define (matrix3-single* a b)
  (let ([m (lambda (r c)
             (element-sum
              (flvector*
               (matrix3-row a r)
               (matrix3-column b c))))])
    (flvector (m 0 0) (m 0 1) (m 0 2)
              (m 1 0) (m 1 1) (m 1 2)
              (m 2 0) (m 2 1) (m 2 2))))

(define (matrix3* . ms)
  (foldl matrix3-single* matrix3-identity ms))

(define (matrix3-vector3* a v)
  (let ([m (lambda (r)
             (element-sum
              (flvector*
               (matrix3-row a r)
               v)))])
    (vector3 (m 0) (m 1) (m 2))))