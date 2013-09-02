#lang racket
(require racket/stream)

(provide true?
         false?
         all
         any
         one
         none
         both
         neither
         either
         one-or-both
         one-or-neither)

(define (true? a)
  (eq? true a))

(define (false? a)
  (not (true? a)))

(define all 
  stream-andmap)

(define any 
  stream-ormap)

(define ((true-count n) f ls)
  (= n (stream-count f ls)))

(define one 
  (true-count 1))

(define (none f ls)
  (not (any f ls)))

(define (both f a b)
  (and (f a) (f b)))

(define (neither f a b)
  (not (one-or-both f a b)))

(define (either f a b)
  (not (or (both f a b)
           (neither f a b))))

(define (one-or-both f a b) 
  (or (f a) (f b)))

(define (one-or-neither f a b)
  (not (both f a b)))
