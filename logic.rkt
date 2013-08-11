#lang lazy
(require racket/stream)

(provide true?)
(provide false?)

(provide all)
(provide any)
(provide one)
(provide none)

(provide both)
(provide neither)
(provide either)
(provide one-or-both)

(define (true? a)
  (eq? true a))

(define (false? a)
  (not (true? a)))

(define all 
  andmap)

(define any 
  ormap)

(define ((true-count n) f ls)
  (= n (stream-count f ls))))

(define one 
  (true-count 1))

(define (none f ls)
  (not (any f ls)))

(define (both f a b)
  (and (f a) (f b)))

(define (neither f a b)
  (not (or (f a)
           (f b))))

(define (either f a b)
  (not (or (both f a b)
           (neither f a b))))

(define (one-or-both f a b) 
  (or (f a) (f b)))