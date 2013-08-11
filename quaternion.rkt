#lang racket
(require math/flonum)
(require "vector3.rkt")

(provide quaternion-identity)
(provide quaternion-inverse)
(provide quaternion-length)
(provide quaternion-normal)
(provide quaternion)
(provide quaternion*)
(provide quaternion-vector*)

(define quaternion-identity
  (flvector 1.0 0.0 0.0 0.0))

(define el 
  flvector-ref)

(define (fl->vector->quaternion f v)
  (list->flvector 
   (cons f (flvector->list v))))

(define (quaternion-vector q)
  (vector3 (el q 1)
           (el q 2)
           (el q 3)))

(define (quaternion a v)
  (fl->vector->quaternion 
   (flcos (fl* 0.5 a))
   (vector3-scale v (flsin (fl* 0.5 a)))))

(define (quaternion-conjugate q)
  (fl->vector->quaternion (el q 0)
                          (vector3- (quaternion-vector q))))

(define (quaternion-length-square q)
  (flvector-sum (flvector-sqr q)))

(define (quaternion-length q)
  (flsqrt (quaternion-length-square q)))

(define (quaternion-inverse q)
  (flvector-scale (quaternion-conjugate q) 
                  (/ (quaternion-length-square q))))

(define (quaternion-normal q)
  (if (zero? (quaternion-length q))
      q
      (flvector-scale q (/ (quaternion-length q)))))

(define (vector->quaternion v)
  (fl->vector->quaternion 0.0 v))

(define (remap-to-vector q m)
  (let ([elm (lambda (q m i) (el q (vector-ref m i)))])
    (vector3 (elm q m 0) 
             (elm q m 1) 
             (elm q m 2))))

(define (row-sum q r)
  (flvector-sum
   (quaternion-conjugate
    (flvector-map (lambda (a . n) (foldl fl* a n))
                  q r))))

(define (col q m r n)
  (vector3-map-mult (remap-to-vector q m) 
                    (remap-to-vector r n)))

(define quaternion* 
  (lambda quats
    (foldl 
     (lambda (r q)
       (let ([a (vector 0 0 0)] 
             [b (vector 1 2 3)]
             [c (vector 2 3 1)]
             [d (vector 3 1 2)])
         (fl->vector->quaternion 
          (row-sum q r)
          (vector3+ (col q a r b)
                    (col q c r d)
                    (col q b r a)
                    (vector3- (col q d r c))))))
     quaternion-identity quats)))

(define (quaternion-vector* q v)
  (quaternion-vector
   (quaternion* q (vector->quaternion v) (conjugate q))))
