#lang typed/racket

(provide air-density)

(struct: gas
  ([constant : Float]
   [molar-mass : Float]))

(define universal-gas-constant 8.314)

(define dry-air
  (gas 287.058 0.028964))

(define water-vapor
  (gas 461.495 0.018016))

(: gas-density (gas -> (Float Float -> Float)))
(define ((gas-density gas) pressure temperature)
  (/ pressure (* temperature (gas-constant gas))))

(define air-density
  (gas-density dry-air))
