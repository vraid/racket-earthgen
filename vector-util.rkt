#lang typed/racket

(require "index-vector.rkt")

(provide index-vector-member)

(: index-vector-member (Fixnum (Vectorof Fixnum) -> maybe-index))
(define (index-vector-member m v)
  (: mem (Integer -> maybe-index))
  (define (mem n)
    (if (= n (vector-length v))
        #f
        (if (eq? m (vector-ref v n))
            n
            (mem (+ n 1)))))
  (mem 0))