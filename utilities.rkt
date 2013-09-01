#lang racket

(require racket/vector)

(provide list-index
         vector-index)

(define (list-index ls a)
  (define (find-index ls a n)
    (if (empty? ls)
        #f
        (if (eq? a (first ls))
            n
            (find-index (rest ls) a (+ n 1)))))
  (find-index ls a 0))

(define (vector-index vs a)
  (define (find-index vs a n)
    (if (= n (vector-length vs))
        #f
        (if (eq? a (vector-ref vs n))
            n
            (find-index vs a (+ n 1)))))
  (find-index vs a 0))