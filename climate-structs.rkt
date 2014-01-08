#lang typed/racket

(provide (struct-out climate-characteristics)
         (struct-out climate-parameters))

(require "typed-struct-kw.rkt")

(struct:/kw climate-characteristics
            ([time-of-year : Flonum]
             [solar-equator : Flonum])
            #:transparent)

(struct:/kw climate-parameters
            ()
            #:transparent)
