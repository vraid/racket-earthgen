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

(define (fxvector-map f v)
  (build-fxvector (fxvector-length v)
                  (lambda (n)
                    (f (fxvector-ref v n)))))

(define (fxvector-member m v)
  (let ([ls (fxvector->list v)])
    (- (length ls) (length (member m ls)))))

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

(define (list->fxvector n)
  (cond ([= 2 n] list->fxvector2)
        ([= 3 n] list->fxvector3)
        ([= 5 n] list->fxvector5)
        ([= 6 n] list->fxvector6)
        (else #f)))

(define (list->fxvector2 ls)
  (let ([el (lambda (n) (vector-ref (list->vector ls) n))])
    (fxvector (el 0)
              (el 1))))

(define (list->fxvector3 ls)
  (let ([el (lambda (n) (vector-ref (list->vector ls) n))])
    (fxvector (el 0)
              (el 1)
              (el 2))))

(define (list->fxvector5 ls)
  (let ([el (lambda (n) (vector-ref (list->vector ls) n))])
    (fxvector (el 0)
              (el 1)
              (el 2)
              (el 3)
              (el 4))))

(define (list->fxvector6 ls)
  (let ([el (lambda (n) (vector-ref (list->vector ls) n))])
    (fxvector (el 0)
              (el 1)
              (el 2)
              (el 3)
              (el 4)
              (el 5))))