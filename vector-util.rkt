#lang typed/racket

(provide vector-index)

(require "types.rkt")

(: vector-index (All (A) (A (Vectorof A) -> index)))
(define (vector-index e v)
  (let ([m (vector-member e v)])
    (if (false? m)
        (vector-length v)
        m)))
