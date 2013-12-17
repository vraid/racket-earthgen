#lang racket

(define mol-mass-water 0.018)
(define mol-mass-dry-air 0.029)
(define gas-constant 8.3144621)
(define gas-constant-dry-air 287.058)

(define (density-gas
         pressure
         temperature
         specific-gas-constant)
  (fl/ pressure
       (fl* specific-gas-constant
            temperature)))

(define (density-dry-air
         pressure
         temperature)
  (density-gas
   pressure
   temperature
   gas-constant-dry-air))

(define (density-humid-air
         
(define (saturation-humidity
         temperature)
  0.0)