#lang typed/racket

(provide quaternion
         quaternion*
         quaternion-vector*
         quaternion-identity
         quaternion-inverse
         quaternion-length
         quaternion-normal
         quaternion->matrix3
         angle-axis->quaternion)

(require math/flonum
         racket/vector
         "vector3.rkt"
         "matrix3.rkt")

(define-type quaternion FlVector)

(: quaternion-identity (-> quaternion))
(define (quaternion-identity)
  (flvector 1.0 0.0 0.0 0.0))

(define el 
  flvector-ref)

(: fl->vector->quaternion (Flonum flvector3 -> quaternion))
(define (fl->vector->quaternion f v)
  (list->flvector 
   (cons f (flvector->list v))))

(: quaternion-vector (quaternion -> flvector3))
(define (quaternion-vector q)
  (flvector (el q 1)
            (el q 2)
            (el q 3)))

(: angle-axis->quaternion (Flonum flvector3 -> quaternion))
(define (angle-axis->quaternion a v)
  (fl->vector->quaternion 
   (flcos (fl* 0.5 a))
   (flvector3-scale v (flsin (fl* 0.5 a)))))

(: quaternion-conjugate (quaternion -> quaternion))
(define (quaternion-conjugate q)
  (fl->vector->quaternion (el q 0)
                          (flvector3- (quaternion-vector q))))

(: quaternion-length-square (quaternion -> Flonum))
(define (quaternion-length-square q)
  (flvector-sum (flvector-sqr q)))

(: quaternion-length (quaternion -> Flonum))
(define (quaternion-length q)
  (flsqrt (quaternion-length-square q)))

(: quaternion-inverse (quaternion -> quaternion))
(define (quaternion-inverse q)
  (flvector-scale (quaternion-conjugate q) 
                  (/ (quaternion-length-square q))))

(: quaternion-normal (quaternion -> quaternion))
(define (quaternion-normal q)
  (if (zero? (quaternion-length q))
      q
      (flvector-scale q (/ (quaternion-length q)))))

(: vector->quaternion (flvector3 -> quaternion))
(define (vector->quaternion v)
  (fl->vector->quaternion 0.0 v))

(: remap-to-vector (quaternion (Vectorof Integer) -> flvector3))
(define (remap-to-vector q m)
  (let ([elm (lambda: ([q : quaternion]
                       [m : (Vectorof Integer)]
                       [i : Integer])
               (el q (vector-ref m i)))])
    (flvector (elm q m 0) 
              (elm q m 1) 
              (elm q m 2))))

(: row-sum (quaternion quaternion -> Flonum))
(define (row-sum q r)
  (: m (Flonum Flonum * -> Flonum))
  (define (m a . n) (foldl * a n))
  (flvector-sum
   (quaternion-conjugate
    (flvector-map m
                  q r))))

(: col (quaternion (Vectorof Integer) quaternion (Vectorof Integer) -> flvector3))
(define (col q m r n)
  (flvector3-map-mult (remap-to-vector q m) 
                      (remap-to-vector r n)))

(: quaternion-single* (quaternion quaternion -> quaternion))
(define (quaternion-single* q r)
  (let ([a (vector 0 0 0)] 
        [b (vector 1 2 3)]
        [c (vector 2 3 1)]
        [d (vector 3 1 2)])
    (fl->vector->quaternion 
     (row-sum q r)
     (flvector3+ (col q a r b)
                 (col q c r d)
                 (col q b r a)
                 (flvector3- (col q d r c))))))

(: quaternion* (quaternion * -> quaternion))
(define (quaternion* . quats)
  (foldl quaternion-single*
         (quaternion-identity) quats))

(: quaternion-vector* (quaternion flvector3 -> flvector3))
(define (quaternion-vector* q v)
  (quaternion-vector
   (quaternion* q (vector->quaternion v) (quaternion-conjugate q))))

(: quaternion->matrix3 (quaternion -> matrix3))
(define (quaternion->matrix3 q)
  (let* ([a (el q 0)]
         [b (el q 1)]
         [c (el q 2)]
         [d (el q 3)]
         [*2 (lambda: ([a : Flonum]
                       [b : Flonum])
               (* 2.0 (* a b)))]
         [*-2 (lambda: ([a : Flonum]
                        [b : Flonum])
                (- (*2 a b)))])
    (matrix3+
     (matrix3-identity)
     (flvector (*-2 c c) (*2 b c) (*2 b d)
               (*2 b c) (*-2 b b) (*2 c d)
               (*2 b d) (*2 c d) (*-2 b b))
     (flvector (*-2 d d) (*-2 a d) (*2 a c)
               (*2 a d) (*-2 d d) (*-2 a b)
               (*-2 a c) (*2 a b) (*-2 c c)))))
