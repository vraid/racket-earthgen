#lang typed/racket

(provide (all-defined-out)
         (struct-out quaternion))

(require "quaternion-local.rkt"
         math/flonum
         "flvector3.rkt"
         "matrix3.rkt")

(struct axis-angle
  ([axis : FlVector]
   [angle : Float])
  #:transparent)

(: quaternion->axis-angle (quaternion -> axis-angle))
(define (quaternion->axis-angle q)
  (let* ([angle (fl* 2.0 (flacos (quaternion-a q)))]
         [scale (let ([s (flsqrt (- 1.0 (flexpt (quaternion-a q) 2.0)))])
                  (lambda: ([el : quaternion-index])
                    (fl/ (el q) s)))]
         [axis (if (zero? angle)
                   (flvector 1.0 0.0 0.0)
                   (flvector (scale i)
                             (scale j)
                             (scale k)))])
    (axis-angle axis angle)))

(: quaternion-identity (-> quaternion))
(define (quaternion-identity)
  (quaternion 1.0 0.0 0.0 0.0))

(: quaternion-to-from (FlVector FlVector -> quaternion))
(define (quaternion-to-from u v)
  (let ([u (flvector3-normal u)]
        [v (flvector3-normal v)])
    (axis-angle->quaternion (flvector3-cross-product u v)
                            (flvector3-angle u v))))

(: quaternion-vector (quaternion -> FlVector))
(define (quaternion-vector q)
  (flvector (i q)
            (j q)
            (k q)))

(: axis-angle->quaternion (FlVector Float -> quaternion))
(define (axis-angle->quaternion v a)
  (fl->vector->quaternion 
   (flcos (fl* 0.5 a))
   (flvector3-scale (flsin (fl* 0.5 a)) (flvector3-normal v))))

(: quaternion-length-square (quaternion -> Float))
(define (quaternion-length-square q)
  (define square (lambda ([a : Float])
                   (* a a)))
  (quaternion-sum (quaternion-map square q)))

(: quaternion-length (quaternion -> Float))
(define (quaternion-length q)
  (flsqrt (quaternion-length-square q)))

(: quaternion-inverse (quaternion -> quaternion))
(define (quaternion-inverse q)
  (quaternion-scale (quaternion-conjugate q) 
                    (/ (quaternion-length-square q))))

(: quaternion-normal (quaternion -> quaternion))
(define (quaternion-normal q)
  (if (zero? (quaternion-length q))
      q
      (quaternion-scale q (/ (quaternion-length q)))))

(: quaternion-product (quaternion * -> quaternion))
(define (quaternion-product . quats)
  (quaternion-normal
   (foldl quaternion-single-product
          (quaternion-identity) quats)))

(: quaternion-vector-product (quaternion FlVector -> FlVector))
(define (quaternion-vector-product q v)
  (quaternion-vector
   (quaternion-product q (vector->quaternion v) (quaternion-conjugate q))))

(: quaternion->matrix3 (quaternion -> FlVector))
(define (quaternion->matrix3 q)
  (let* ([a (a q)]
         [b (i q)]
         [c (j q)]
         [d (k q)]
         [*2 (lambda: ([a : Float]
                       [b : Float])
               (* 2.0 (* a b)))]
         [*-2 (lambda: ([a : Float]
                        [b : Float])
                (- (*2 a b)))])
    (matrix3+
     (matrix3-identity)
     (flvector (*-2 c c) (*2 b c) (*2 b d)
               (*2 b c) (*-2 b b) (*2 c d)
               (*2 b d) (*2 c d) (*-2 b b))
     (flvector (*-2 d d) (*-2 a d) (*2 a c)
               (*2 a d) (*-2 d d) (*-2 a b)
               (*-2 a c) (*2 a b) (*-2 c c)))))
