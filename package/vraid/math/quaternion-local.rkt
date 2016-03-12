#lang typed/racket

(provide (all-defined-out))

(require "flvector3.rkt"
         math/flonum)

(define-type quaternion-index (quaternion -> Float))
(define-type quaternion-index-vector (Vectorof quaternion-index))

(struct quaternion
  ([a : Flonum]
   [i : Flonum]
   [j : Flonum]
   [k : Flonum])
  #:transparent)

(define a quaternion-a)
(define i quaternion-i)
(define j quaternion-j)
(define k quaternion-k)

(: remap-to-vector (quaternion quaternion-index-vector -> FlVector))
(define (remap-to-vector q m)
  (let ([elm (lambda: ([q : quaternion]
                       [m : quaternion-index-vector]
                       [i : Integer])
               ((vector-ref m i) q))])
    (flvector (elm q m 0) 
              (elm q m 1) 
              (elm q m 2))))

(: col (quaternion quaternion-index-vector quaternion quaternion-index-vector -> FlVector))
(define (col q m r n)
  (flvector3-map-mult (remap-to-vector q m) 
                      (remap-to-vector r n)))
