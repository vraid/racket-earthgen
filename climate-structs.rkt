#lang typed/racket

(require "typed-struct-kw.rkt")

(provide planet-characteristics
         planet-characteristics/kw
         planet-characteristics-axial-tilt
         planet-characteristics-radius
         planet-characteristics-seasons
         climate-characteristics
         climate-characteristics/kw
         climate-characteristics-time-of-year
         climate-characteristics-solar-equator
         climate-parameters
         climate-parameters/kw)

(struct:/kw planet-characteristics
  ([axial-tilt : Flonum]
   [radius : Flonum]
   [seasons : Integer])
  #:transparent)

(struct:/kw climate-parameters
  ()
  #:transparent)

(struct:/kw climate-characteristics
  ([time-of-year : Flonum]
   [solar-equator : Flonum])
  #:transparent)
