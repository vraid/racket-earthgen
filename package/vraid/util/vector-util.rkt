#lang typed/racket

(require math/flonum)

(provide (all-defined-out))

(: vector-index (All (a) (a (Vectorof a) -> Integer)))
(define (vector-index e v)
  (let ([m (vector-member e v)])
    (if (false? m)
        (vector-length v)
        m)))

(: build-vector-ref (All (a) (Integer (Integer -> a) -> (Integer -> a))))
(define (build-vector-ref count f)
  (let ([v (build-vector count f)])
    (λ ([n : Integer])
      (vector-ref v n))))

(: build-flvector-ref (Integer (Integer -> Float) -> (Integer -> Float)))
(define (build-flvector-ref count f)
  (let ([v (build-flvector count f)])
    (λ ([n : Integer])
      (flvector-ref v n))))

(: vector-take-at-most (All (a) ((Vectorof a) Integer -> (Vectorof a))))
(define (vector-take-at-most vec n)
  (vector-take vec (min n (vector-length vec))))

(struct (A) vector-accessor
  ([get : (Integer -> A)]
   [set : (Integer A -> Void)]))

(: vector-get (All (A) ((Vectorof A) -> (Integer -> A))))
(define ((vector-get v) n)
  (vector-ref v n))

(: vector-set (All (A) ((Vectorof A) -> (Integer A -> Void))))
(define ((vector-set v) n value)
  (vector-set! v n value))

(: flvector-get (FlVector -> (Integer -> Float)))
(define ((flvector-get v) n)
  (flvector-ref v n))

(: flvector-set (FlVector -> (Integer Float -> Void)))
(define ((flvector-set v) n value)
  (flvector-set! v n value))

(: flvector-add! (FlVector Integer Float -> Void))
(define (flvector-add! v n a)
  (flvector-set! v n (fl+ a (flvector-ref v n))))

(: make-vector-accessor (All (A) ((Vectorof A) -> (vector-accessor A))))
(define (make-vector-accessor v)
  (vector-accessor
   (vector-get v)
   (vector-set v)))

(: make-flvector-accessor (FlVector -> (vector-accessor Float)))
(define (make-flvector-accessor v)
  (vector-accessor
   (flvector-get v)
   (flvector-set v)))

(: build-vector-accessor (All (A) (Integer (Integer -> A) -> (vector-accessor A))))
(define (build-vector-accessor n get)
  (make-vector-accessor (build-vector n get)))

(: build-flvector-accessor (Integer (Integer -> Float) -> (vector-accessor Float)))
(define (build-flvector-accessor n get)
  (make-flvector-accessor (build-flvector n get)))
