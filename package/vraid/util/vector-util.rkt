#lang typed/racket

(provide (all-defined-out))

(require "../types.rkt"
         math/flonum)

(: vector-index (All (a) (a (Vectorof a) -> integer)))
(define (vector-index e v)
  (let ([m (vector-member e v)])
    (if (false? m)
        (vector-length v)
        m)))

(: build-vector-ref (All (a) (integer (integer -> a) -> (integer -> a))))
(define (build-vector-ref count f)
  (let ([v (build-vector count f)])
    (lambda: ([n : integer])
      (vector-ref v n))))

(: build-flvector-ref (integer (integer -> flonum) -> (integer -> flonum)))
(define (build-flvector-ref count f)
  (let ([v (build-flvector count f)])
    (lambda: ([n : integer])
      (flvector-ref v n))))

(: vector-take-at-most (All (a) ((Vectorof a) integer -> (Vectorof a))))
(define (vector-take-at-most vec n)
  (vector-take vec (min n (vector-length vec))))
