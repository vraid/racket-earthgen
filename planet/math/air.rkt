#lang typed/racket

(provide air-density)

(require vraid/types
         plot/typed)

(struct: gas
  ([constant : flonum]
   [molar-mass : flonum]))

(define universal-gas-constant 8.314)

(define dry-air
  (gas 287.058 0.028964))

(define water-vapor
  (gas 461.495 0.018016))

(: gas-density (gas -> (flonum flonum -> flonum)))
(define ((gas-density gas) pressure temperature)
  (/ pressure (* temperature (gas-constant gas))))

(define air-density
  (gas-density dry-air))