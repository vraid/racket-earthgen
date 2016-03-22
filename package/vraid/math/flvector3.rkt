#lang typed/racket

(provide (all-defined-out))

(require "flvector3-local.rkt"
         "common.rkt"
         math/flonum)

(: flvector3-zero (-> FlVector))
(define (flvector3-zero)
  (flvector 0.0 0.0 0.0))

(: flvector3-zero? (FlVector -> Boolean))
(define (flvector3-zero? v)
  (zero? (flvector3-length-squared v)))

(: flvector3-negative (FlVector -> FlVector))
(define (flvector3-negative v)
  (flvector3-scale -1.0 v))

(: flvector3-length-squared (FlVector -> Float))
(define (flvector3-length-squared v)
  (flvector-sum
   (flvector-sqr v)))

(: flvector3-length (FlVector -> Float))
(define (flvector3-length v)
  (flsqrt 
   (flvector3-length-squared v)))

(: flvector3-distance-squared (FlVector FlVector -> Float))
(define (flvector3-distance-squared u v)
  (flvector3-length-squared (flvector3-subtract u v)))

(: flvector3-distance (FlVector FlVector -> Float))
(define (flvector3-distance u v)
  (flvector3-length (flvector3-subtract u v)))

(: flvector3-scale (Float FlVector -> FlVector))
(define (flvector3-scale a v)
  (flvector-scale v a))

(: flvector3-scale-to (Float FlVector -> FlVector))
(define (flvector3-scale-to a v)
  (flvector3-scale (divide-by (flvector3-length v)
                              a)
                   v))

(: flvector3-normal (FlVector -> FlVector))
(define (flvector3-normal a)
  (if (zero? (flvector3-length-squared a))
      a
      (flvector3-scale (/ (flvector3-length a)) a)))

(: flvector3-sum (FlVector * -> FlVector))
(define (flvector3-sum . vecs)
  (foldl flvector+
         (flvector3-zero) vecs))

(: flvector3-subtract (FlVector FlVector -> FlVector))
(define (flvector3-subtract a b)
  (flvector- b a))

(: flvector3-map-mult (FlVector * -> FlVector))
(define (flvector3-map-mult . vecs)
  (foldl (lambda: ([v : FlVector]
                   [u : FlVector])
           (flvector-map * v u))
         (flvector 1.0 1.0 1.0) vecs))

(: flvector3-dot-product (FlVector FlVector -> Float))
(define (flvector3-dot-product u v)
  (flvector-sum
   (flvector-map * u v)))

(: flvector3-cross-product (FlVector FlVector -> FlVector))
(define (flvector3-cross-product u v)
  (let* ([m (vector 1 2 0)]
         [n (vector 2 0 1)])
    (flvector3-subtract (col u n v m)
                        (col u m v n))))

(: flvector3-angle (FlVector FlVector -> Float))
(define (flvector3-angle a b)
  (flacos (fl/ (flvector3-dot-product a b)
               (flsqrt (* (flvector3-length-squared a)
                          (flvector3-length-squared b))))))

(: flvector3-projection (FlVector FlVector -> FlVector))
(define (flvector3-projection target v)
  (let ([n (flvector3-normal target)])
    (flvector3-scale (flvector3-dot-product n v) n)))

(: flvector3-rejection (FlVector FlVector -> FlVector))
(define (flvector3-rejection rejector v)
  (flvector3-subtract (flvector3-projection rejector v) v))

(: flvector3-parallel? (FlVector FlVector -> Boolean))
(define (flvector3-parallel? u v)
  (zero? (flvector-sum (flvector3-cross-product u v))))
