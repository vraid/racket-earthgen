#lang racket

(provide add
         subtract
         multiply
         divide)

(define (add n)
  (lambda (k)
    (+ k n)))

(define (subtract n)
  (lambda (k)
    (- k n)))

(define (multiply n)
  (lambda (k)
    (* k n)))

(define (divide n)
  (lambda (k)
    (/ k n)))
