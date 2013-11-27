#lang typed/racket

(require "types.rkt"
         "index-vector.rkt")

(provide vector-member
         vector-index)

(: vector-member (All (A) (A (Vectorof A) -> maybe-index)))
(define (vector-member m v)
  (: mem (index -> maybe-index))
  (define (mem n)
    (if (= n (vector-length v))
        #f
        (if (eq? m (vector-ref v n))
            n
            (mem (+ n 1)))))
  (mem 0))

(: vector-index (All (A) (A (Vectorof A) -> index)))
(define (vector-index m v)
  (: in (index -> index))
  (define (in n)
    (if (= n (vector-length v))
        (+ 1 (vector-length v))
        (if (eq? m (vector-ref v n))
            n
            (in (+ n 1)))))
  (in 0))

