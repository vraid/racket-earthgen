#lang typed/racket

(provide (all-defined-out))

(require math/flonum)

(: vector-index (All (a) (a (Vectorof a) -> Integer)))
(define (vector-index e v)
  (let ([m (vector-member e v)])
    (if (false? m)
        (vector-length v)
        m)))

(: build-vector-ref (All (a) (Integer (Integer -> a) -> (Integer -> a))))
(define (build-vector-ref count f)
  (let ([v (build-vector count f)])
    (lambda: ([n : Integer])
      (vector-ref v n))))

(: build-flvector-ref (Integer (Integer -> Float) -> (Integer -> Float)))
(define (build-flvector-ref count f)
  (let ([v (build-flvector count f)])
    (lambda: ([n : Integer])
      (flvector-ref v n))))

(: vector-take-at-most (All (a) ((Vectorof a) Integer -> (Vectorof a))))
(define (vector-take-at-most vec n)
  (vector-take vec (min n (vector-length vec))))
