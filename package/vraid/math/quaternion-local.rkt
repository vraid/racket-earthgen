#lang typed/racket

(provide (all-defined-out))

(require "flvector3.rkt"
         math/flonum)

(define-type quaternion-index (quaternion -> Float))
(define-type quaternion-index-vector (Vectorof quaternion-index))

(struct quaternion
  ([a : Float]
   [i : Float]
   [j : Float]
   [k : Float])
  #:transparent)

(define a quaternion-a)
(define i quaternion-i)
(define j quaternion-j)
(define k quaternion-k)

(: fl->vector->quaternion (Float FlVector -> quaternion))
(define (fl->vector->quaternion f v)
  (quaternion
   f
   (flvector-ref v 0)
   (flvector-ref v 1)
   (flvector-ref v 2)))

(: vector->quaternion (FlVector -> quaternion))
(define (vector->quaternion v)
  (fl->vector->quaternion 0.0 v))

(: quaternion-map ((Float -> Float) quaternion -> quaternion))
(define (quaternion-map f q)
  (quaternion (f (a q))
              (f (i q))
              (f (j q))
              (f (k q))))

(: quaternion-map2 ((Float Float -> Float) quaternion quaternion -> quaternion))
(define (quaternion-map2 f q r)
  (quaternion (f (a q) (a r))
              (f (i q) (i r))
              (f (j q) (j r))
              (f (k q) (k r))))

(: quaternion-scale (quaternion Float -> quaternion))
(define (quaternion-scale q scale)
  (quaternion
   (* scale (a q))
   (* scale (i q))
   (* scale (j q))
   (* scale (k q))))

(: quaternion-sum (quaternion -> Float))
(define (quaternion-sum q)
  (+ (a q) (i q) (j q) (k q)))

(: quaternion-conjugate (quaternion -> quaternion))
(define (quaternion-conjugate q)
  (quaternion (a q)
              (- (i q))
              (- (j q))
              (- (k q))))

(: quaternion-row-sum (quaternion quaternion -> Float))
(define (quaternion-row-sum q r)
  (quaternion-sum
   (quaternion-conjugate
    (quaternion-map2 * q r))))

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

(: remap-to-vector (quaternion quaternion-index-vector -> FlVector))
(define (remap-to-vector q m)
  (let ([elm (lambda: ([q : quaternion]
                       [m : quaternion-index-vector]
                       [i : Integer])
               ((vector-ref m i) q))])
    (flvector (elm q m 0) 
              (elm q m 1) 
              (elm q m 2))))

(: col (quaternion quaternion-index-vector quaternion quaternion-index-vector -> FlVector))
(define (col q m r n)
  (flvector3-map-mult (remap-to-vector q m) 
                      (remap-to-vector r n)))
