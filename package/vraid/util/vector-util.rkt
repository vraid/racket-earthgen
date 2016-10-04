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

(struct (A) vector-accessor
  ([get : (Integer -> A)]
   [set : (Integer A -> Void)]))

(: make-vector-accessor (All (A) ((Vectorof A) -> (vector-accessor A))))
(define (make-vector-accessor v)
  (vector-accessor (lambda ([n : Integer])
                   (vector-ref v n))
                 (lambda ([n : Integer]
                          [a : A])
                   (vector-set! v n a))))

(: make-flvector-accessor (FlVector -> (vector-accessor Float)))
(define (make-flvector-accessor v)
  (vector-accessor (curry flvector-ref v)
                 (lambda ([n : Integer]
                          [a : Float])
                   (flvector-set! v n a))))
