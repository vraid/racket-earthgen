#lang racket
(require math/flonum)
(require racket/vector)

(provide vector3)
(provide vector3-identity)
(provide vector3-scale)
(provide vector3-length)
(provide vector3-normal)
(provide vector3+)
(provide vector3-)
(provide vector3-dot-product)
(provide vector3-cross-product)
(provide vector3-map-mult)

(define vector3 (lambda (a b c) (flvector a b c)))

(define vector3-identity (flvector 0.0 0.0 0.0))

(define vector3-length (lambda (v) (flsqrt (flvector-sum (flvector-map (lambda (a) (flexpt a 2.0)) v)))))

(define vector3-scale flvector-scale)

(define vector3-normal (lambda (a) (if (zero? (vector3-length a)) a
                                       (vector3-scale a (/ (vector3-length a))))))

(define vector3+ (lambda vecs (foldl flvector+ vector3-identity vecs)))

(define vector3- (lambda (v . vecs) (if (null? vecs) (flvector- v)
                                        (foldl (lambda (b a) (flvector- a b)) v vecs))))

(define mult (lambda (a . n) (foldl fl* a n)))

(define vector3-map-mult (lambda vecs (foldl (lambda (v u) (flvector-map mult v u)) (vector3 1.0 1.0 1.0) vecs)))

(define vector3-dot-product (lambda (v u) (flvector-sum (flvector-map mult v u))))

(define vector3-cross-product (lambda (v u) (let ([m (vector 1 2 0)] [n (vector 2 0 1)]
                                                  [remap (lambda (v m) (let ([elm (lambda (v m i) (flvector-ref v (vector-ref m i)))])
                                                                         (vector3 (elm v m 0) (elm v m 1) (elm v m 2))))])
                                              (vector3- (flvector-map mult (remap v m) (remap u n)) (flvector-map mult (remap v n) (remap u m))))))
                                