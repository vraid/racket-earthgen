#lang typed/racket

(provide (all-defined-out))

(require "quaternion-local.rkt"
         math/flonum
         "flvector3.rkt"
         "matrix3.rkt")

(: quaternion-identity (-> quaternion))
(define (quaternion-identity)
  (flvector 1.0 0.0 0.0 0.0))

(: fl->vector->quaternion (Flonum flvector3 -> quaternion))
(define (fl->vector->quaternion f v)
  (list->flvector 
   (cons f (flvector->list v))))

(: quaternion-vector (quaternion -> flvector3))
(define (quaternion-vector q)
  (flvector (el q 1)
            (el q 2)
            (el q 3)))

(: axis-angle->quaternion (flvector3 Flonum -> quaternion))
(define (axis-angle->quaternion v a)
  (fl->vector->quaternion 
   (flcos (fl* 0.5 a))
   (flvector3-scale (flsin (fl* 0.5 a)) v)))

(: quaternion-conjugate (quaternion -> quaternion))
(define (quaternion-conjugate q)
  (fl->vector->quaternion (el q 0)
                          (flvector3-negative (quaternion-vector q))))

(: quaternion-length-square (quaternion -> Flonum))
(define (quaternion-length-square q)
  (flvector-sum (flvector-sqr q)))

(: quaternion-length (quaternion -> Flonum))
(define (quaternion-length q)
  (flsqrt (quaternion-length-square q)))

(: quaternion-inverse (quaternion -> quaternion))
(define (quaternion-inverse q)
  (flvector-scale (quaternion-conjugate q) 
                  (/ (quaternion-length-square q))))

(: quaternion-normal (quaternion -> quaternion))
(define (quaternion-normal q)
  (if (zero? (quaternion-length q))
      q
      (flvector-scale q (/ (quaternion-length q)))))

(: vector->quaternion (flvector3 -> quaternion))
(define (vector->quaternion v)
  (fl->vector->quaternion 0.0 v))

(: quaternion-row-sum (quaternion quaternion -> Flonum))
(define (quaternion-row-sum q r)
  (: m (Flonum Flonum * -> Flonum))
  (define (m a . n) (foldl * a n))
  (flvector-sum
   (quaternion-conjugate
    (flvector-map m
                  q r))))

(: quaternion-single-product (quaternion quaternion -> quaternion))
(define (quaternion-single-product q r)
  (let ([a (vector 0 0 0)] 
        [b (vector 1 2 3)]
        [c (vector 2 3 1)]
        [d (vector 3 1 2)])
    (fl->vector->quaternion 
     (quaternion-row-sum q r)
     (flvector3-sum (col q a r b)
                    (col q c r d)
                    (col q b r a)
                    (flvector3-negative (col q d r c))))))

(: quaternion-product (quaternion * -> quaternion))
(define (quaternion-product . quats)
  (foldl quaternion-single-product
         (quaternion-identity) quats))

(: quaternion-vector-product (quaternion flvector3 -> flvector3))
(define (quaternion-vector-product q v)
  (quaternion-vector
   (quaternion-product q (vector->quaternion v) (quaternion-conjugate q))))

(: quaternion->matrix3 (quaternion -> matrix3))
(define (quaternion->matrix3 q)
  (let* ([a (el q 0)]
         [b (el q 1)]
         [c (el q 2)]
         [d (el q 3)]
         [*2 (lambda: ([a : Flonum]
                       [b : Flonum])
               (* 2.0 (* a b)))]
         [*-2 (lambda: ([a : Flonum]
                        [b : Flonum])
                (- (*2 a b)))])
    (matrix3+
     (matrix3-identity)
     (flvector (*-2 c c) (*2 b c) (*2 b d)
               (*2 b c) (*-2 b b) (*2 c d)
               (*2 b d) (*2 c d) (*-2 b b))
     (flvector (*-2 d d) (*-2 a d) (*2 a c)
               (*2 a d) (*-2 d d) (*-2 a b)
               (*-2 a c) (*2 a b) (*-2 c c)))))
