#lang racket

(require racket/fixnum)

(provide fxvector->list
         list->fxvector)

(define (fxvector->list v)
  (map (lambda (n) (fxvector-ref v n))
       (range 0 (fxvector-length v))))

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