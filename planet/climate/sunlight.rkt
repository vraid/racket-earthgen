#lang typed/racket

(provide (all-defined-out))

(require math/flonum)

(define solar-constant 1361.0) ; joule / s / m^2

(: sunlight (Float Float -> Float))
(define (sunlight solar-equator latitude)
  (* solar-constant
     (max 0.0 (flcos (fl- solar-equator latitude)))))

(: cloud-cover (Float -> Float))
(define (cloud-cover relative-humidity)
  (* 0.5 relative-humidity))

(: insolation (Float Float -> Float))
(define (insolation sunlight cloud-cover)
  (* sunlight (- 1.0 cloud-cover)))
