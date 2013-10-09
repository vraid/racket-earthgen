#lang typed/racket

(require racket/fixnum)

(provide fixtype
         fixtype?
         fixvector
         fxvector->list
         list->fxvector
         fxvector-map
         build-fxvector
         fxvector-member)

(define-type fixtype Fixnum)
(define fixtype? fixnum?)
(define-type fixvector (Vectorof fixtype))

(: fxvector->list (fixvector -> (Listof fixtype)))
(define (fxvector->list v)
  (map (lambda: ([n : fixtype]) (vector-ref v n))
       (range 0 (vector-length v))))

(: list->fxvector (fixtype (Listof fixtype) -> fixvector))
(define (list->fxvector n ls)
  (let ([v (list->vector ls)])
    (build-fxvector n (lambda: ([n : fixtype])
                        (let ([num (vector-ref v n)])
                          (if (fixtype? num)
                              num
                              0))))))

(: fxvector-map ((fixtype -> fixtype) fixvector -> fixvector))
(define (fxvector-map f v)
  (build-fxvector
   (vector-length v)
   (lambda: ([n : fixtype]) (f (vector-ref v n)))))

(: fxvector-member (fixtype fixvector -> (U Boolean fixtype)))
(define (fxvector-member m v)
  (: mem (fixtype -> (U Boolean fixtype)))
  (define (mem n)
    (if (= n (vector-length v))
        #f
        (if (eq? m (vector-ref v n))
            n
            (mem (fx+ n 1)))))
  (mem 0))

(: build-fxvector (fixtype (fixtype -> fixtype) -> fixvector))
(define (build-fxvector n f)
  (cond ([= 2 n] (build-fxvector2 f))
        ([= 3 n] (build-fxvector3 f))
        ([= 5 n] (build-fxvector5 f))
        ([= 6 n] (build-fxvector6 f))
        (else (vector))))

(: build-fxvector2 ((Fixnum -> Fixnum) -> fixvector))
(define (build-fxvector2 f)
  (vector (f 0)
          (f 1)))

(: build-fxvector3 ((Fixnum -> Fixnum) -> fixvector))
(define (build-fxvector3 f)
  (vector (f 0)
          (f 1)
          (f 2)))

(: build-fxvector5 ((Fixnum -> Fixnum) -> fixvector))
(define (build-fxvector5 f)
  (vector (f 0)
          (f 1)
          (f 2)
          (f 3)
          (f 4)))

(: build-fxvector6 ((Fixnum -> Fixnum) -> fixvector))
(define (build-fxvector6 f)
  (vector (f 0)
          (f 1)
          (f 2)
          (f 3)
          (f 4)
          (f 5)))
