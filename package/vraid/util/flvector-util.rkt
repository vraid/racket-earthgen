#lang typed/racket

(require math/flonum)

(: build-flvector-ref (Positive-Integer (Integer -> Flonum) -> (Integer -> Flonum)))
(define (build-flvector-ref count f)
  (let ([v (build-flvector count f)])
    (lambda: ([n : Integer])
      (flvector-ref v n))))
