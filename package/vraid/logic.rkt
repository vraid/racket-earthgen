#lang typed/racket

(provide (all-defined-out))

(: truify (Any -> Boolean))
(define (truify a)
  (if a #t #f))
