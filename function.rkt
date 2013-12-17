#lang typed/racket

(provide flip)

(: flip (All (a b c) ((a b -> c) -> (b a -> c))))
(define (flip fn)
  (lambda (a b)
    (fn b a)))
