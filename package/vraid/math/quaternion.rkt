#lang typed/racket

(provide (all-defined-out)
         (struct-out quaternion))

(require "quaternion-local.rkt"
         math/flonum
         "flvector3.rkt"
         "matrix3.rkt")

(struct axis-angle
  ([axis : FlVector]
   [angle : Float])
  #:transparent)

(: quaternion->axis-angle (quaternion -> axis-angle))
(define (quaternion->axis-angle q)
  (let* ([angle (fl* 2.0 (flacos (quaternion-a q)))]
         [scale (let ([s (flsqrt (- 1.0 (flexpt (quaternion-a q) 2.0)))])
                  (lambda: ([el : quaternion-index])
                    (fl/ (el q) s)))]
         [axis (if (= 1 angle)
                   (flvector 1.0 0.0 0.0)
                   (flvector (scale i)
                             (scale j)
                             (scale k)))])
    (axis-angle axis angle)))

(: quaternion-identity (-> quaternion))
(define (quaternion-identity)
  (quaternion 1.0 0.0 0.0 0.0))

(: quaternion-to-from (FlVector FlVector -> quaternion))
(define (quaternion-to-from u v)
  (let ([u (flvector3-normal u)]
        [v (flvector3-normal v)])
    (axis-angle->quaternion (flvector3-cross-product u v)
                            (flvector3-angle u v))))

(: flvector->quaternion (FlVector -> quaternion))
(define (flvector->quaternion v)
  (quaternion
   (flvector-ref v 0)
   (flvector-ref v 1)
   (flvector-ref v 2)
   (flvector-ref v 3)))

(: quaternion->flvector (quaternion -> FlVector))
(define (quaternion->flvector q)
  (flvector (a q) (i q) (j q) (k q)))

(: fl->vector->quaternion (Float FlVector -> quaternion))
(define (fl->vector->quaternion f v)
  (quaternion
   f
   (flvector-ref v 0)
   (flvector-ref v 1)
   (flvector-ref v 2)))

(: quaternion-vector (quaternion -> FlVector))
(define (quaternion-vector q)
  (flvector (i q)
            (j q)
            (k q)))

(: axis-angle->quaternion (FlVector Float -> quaternion))
(define (axis-angle->quaternion v a)
  (fl->vector->quaternion 
   (flcos (fl* 0.5 a))
   (flvector3-scale (flsin (fl* 0.5 a)) (flvector3-normal v))))

(: quaternion-sum (quaternion -> Float))
(define (quaternion-sum q)
  (+ (a q) (i q) (j q) (k q)))

(: quaternion-conjugate (quaternion -> quaternion))
(define (quaternion-conjugate q)
  (quaternion (a q)
              (- (i q))
              (- (j q))
              (- (k q))))

(: quaternion-length-square (quaternion -> Float))
(define (quaternion-length-square q)
  (flvector-sum (flvector-sqr (quaternion->flvector q))))

(: quaternion-length (quaternion -> Float))
(define (quaternion-length q)
  (flsqrt (quaternion-length-square q)))

(: quaternion-scale (quaternion Float -> quaternion))
(define (quaternion-scale q scale)
  (quaternion
   (* scale (a q))
   (* scale (i q))
   (* scale (j q))
   (* scale (k q))))

(: quaternion-inverse (quaternion -> quaternion))
(define (quaternion-inverse q)
  (quaternion-scale (quaternion-conjugate q) 
                    (/ (quaternion-length-square q))))

(: quaternion-normal (quaternion -> quaternion))
(define (quaternion-normal q)
  (if (zero? (quaternion-length q))
      q
      (quaternion-scale q (/ (quaternion-length q)))))

(: vector->quaternion (FlVector -> quaternion))
(define (vector->quaternion v)
  (fl->vector->quaternion 0.0 v))

(: quaternion-row-sum (quaternion quaternion -> Float))
(define (quaternion-row-sum q r)
  (: m (Float Float * -> Float))
  (define (m a . n) (foldl * a n))
  (quaternion-sum
   (quaternion-conjugate
    (flvector->quaternion
     (flvector-map m
                   (quaternion->flvector q)
                   (quaternion->flvector r))))))

(: quaternion-single-product (quaternion quaternion -> quaternion))
(define quaternion-single-product
  (let ([a : quaternion-index-vector (vector a a a)] 
        [b : quaternion-index-vector (vector i j k)]
        [c : quaternion-index-vector (vector j k i)]
        [d : quaternion-index-vector (vector k i j)])
    (lambda ([q : quaternion]
             [r : quaternion])
      (fl->vector->quaternion 
       (quaternion-row-sum q r)
       (flvector3-sum (col q a r b)
                      (col q c r d)
                      (col q b r a)
                      (flvector3-negative (col q d r c)))))))

(: quaternion-product (quaternion * -> quaternion))
(define (quaternion-product . quats)
  (quaternion-normal
   (foldl quaternion-single-product
          (quaternion-identity) quats)))

(: quaternion-vector-product (quaternion FlVector -> FlVector))
(define (quaternion-vector-product q v)
  (quaternion-vector
   (quaternion-product q (vector->quaternion v) (quaternion-conjugate q))))

(: quaternion->matrix3 (quaternion -> FlVector))
(define (quaternion->matrix3 q)
  (let* ([a (a q)]
         [b (i q)]
         [c (j q)]
         [d (k q)]
         [*2 (lambda: ([a : Float]
                       [b : Float])
               (* 2.0 (* a b)))]
         [*-2 (lambda: ([a : Float]
                        [b : Float])
                (- (*2 a b)))])
    (matrix3+
     (matrix3-identity)
     (flvector (*-2 c c) (*2 b c) (*2 b d)
               (*2 b c) (*-2 b b) (*2 c d)
               (*2 b d) (*2 c d) (*-2 b b))
     (flvector (*-2 d d) (*-2 a d) (*2 a c)
               (*2 a d) (*-2 d d) (*-2 a b)
               (*-2 a c) (*2 a b) (*-2 c c)))))
