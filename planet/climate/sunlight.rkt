#lang typed/racket

(provide (all-defined-out))

(require vraid/types
         math/flonum)

(define solar-constant 1361.0) ; joule / s / m^2

(: sunlight (flonum flonum -> flonum))
(define (sunlight solar-equator latitude)
  (* solar-constant
     (max 0.0 (flcos (fl- solar-equator latitude)))))

(: cloud-cover (flonum -> flonum))
(define (cloud-cover relative-humidity)
  (* 0.5 relative-humidity))

(: insolation (flonum flonum -> flonum))
(define (insolation sunlight cloud-cover)
  (* sunlight (- 1.0 cloud-cover)))
