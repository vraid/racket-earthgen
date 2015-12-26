#lang typed/racket

(provide (all-defined-out))

(require racket/flonum)

(: tau Flonum)
(define tau (* 2.0 pi))
