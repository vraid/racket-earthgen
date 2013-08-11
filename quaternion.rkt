#lang racket
(require math/flonum)
(require "vector3.rkt")

(provide quaternion-identity)
(provide quaternion-conjugate)
(provide quaternion-length)
(provide quaternion-normal)
(provide quaternion)
(provide quaternion*)
(provide quaternion-vector*)

(define quaternion-identity (flvector 1.0 0.0 0.0 0.0))

(define el flvector-ref)

(define fl->vector->quaternion (lambda (f v) (list->flvector (cons f (flvector->list v)))))

(define quaternion-vector (lambda (q) (vector3 (el q 1) (el q 2) (el q 3))))

(define quaternion (lambda (a v) (fl->vector->quaternion (flcos (fl* 0.5 a)) (vector3-scale v (flsin (fl* 0.5 a))))))

(define quaternion-conjugate (lambda (q) (fl->vector->quaternion (el q 0) (vector3- (quaternion-vector q)))))

(define quaternion-length (lambda (q) (flsqrt (flvector-sum (flvector-map (lambda (a) (flexpt a 2.0)) q)))))

(define quaternion-normal (lambda (q) (if (flsubnormal? (quaternion-length q)) q
                                          (flvector-scale (fl/ (quaternion-length q))))))

(define vector->quaternion (lambda (v) (fl->vector->quaternion 0.0 v)))

(define remap (lambda (q m) (let ([elm (lambda (q m i) (el q (vector-ref m i)))])
                              (vector3 (elm q m 0) (elm q m 1) (elm q m 2)))))

(define quaternion* (lambda quats (foldl (lambda (r q) (let ([a (vector 0 0 0)] [b (vector 1 2 3)] [c (vector 2 3 1)] [d (vector 3 1 2)]
                                                             [v (quaternion-vector q)] [u (quaternion-vector r)]
                                                             [col (lambda (v m u n) (vector3-map-mult (remap v m) (remap u n)))])
                                                         (fl->vector->quaternion (fl- (fl* (el q 0) (el r 0)) (vector3-dot-product v u))
                                                                                 (vector3- (vector3+ (col q a r b)
                                                                                                     (col q c r d)
                                                                                                     (col q b r a))
                                                                                           (col q d r c)))))
                                         quaternion-identity quats)))

(define quaternion-vector* (lambda (q v) (quaternion-vector (quaternion* q (vector->quaternion v) (conjugate q)))))