#lang typed/racket

(provide (all-defined-out))

(require racket/flonum)

(: pi Flonum)
(define pi (flacos -1.0))

(: tau Flonum)
(define tau (* 2.0 pi))
