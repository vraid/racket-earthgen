#lang racket

(require racket/fixnum)

(provide fxvector->list
         list->fxvector
         fxvector-map
         build-fxvector
         fxvector-member)

(define (fxvector->list v)
  (map (lambda (n) (fxvector-ref v n))
       (range 0 (fxvector-length v))))

(define (list->fxvector n ls)
  (let ([v (list->vector ls)])
    (build-fxvector n (lambda (n) (vector-ref v n)))))

(define (fxvector-map f v)
  (build-fxvector
   (fxvector-length v)
   (lambda (n) (f (fxvector-ref v n)))))

(define (fxvector-member m v)
  (define (mem n)
    (if (= n (fxvector-length v))
        #f
        (if (eq? m (fxvector-ref v n))
            n
            (mem (+ n 1)))))
  (mem 0))

(define (build-fxvector n f)
  (cond ([= 2 n] (build-fxvector2 f))
        ([= 3 n] (build-fxvector3 f))
        ([= 5 n] (build-fxvector5 f))
        ([= 6 n] (build-fxvector6 f))
        (else #f)))

(define (build-fxvector2 f)
  (fxvector (f 0)
            (f 1)))
  
(define (build-fxvector3 f)
  (fxvector (f 0)
            (f 1)
            (f 2)))
  
(define (build-fxvector5 f)
  (fxvector (f 0)
            (f 1)
            (f 2)
            (f 3)
            (f 4)))
  
(define (build-fxvector6 f)
  (fxvector (f 0)
            (f 1)
            (f 2)
            (f 3)
            (f 4)
            (f 5)))
