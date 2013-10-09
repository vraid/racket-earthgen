#lang typed/racket

(provide true?
         false?
         any
         all
         none
         one
         both
         neither
         either
         one-or-both
         one-or-neither)

(define true?
  (lambda: ([a : Boolean])
    (eq? true a)))

(define false?
  (lambda: ([a : Boolean])
    (not (true? a))))

(define any ormap)

(define all andmap)

(: none (All (A) ((A -> Boolean) (Listof A) -> Boolean)))
(define (none f ls)
  (not (any f ls)))

(define true-count
  (lambda: ([n : Integer])
    (ann (lambda (f ls)
           (eq? n (count f ls)))
         (All (A) ((A -> Boolean) (Listof A) -> Boolean)))))

(: one (All (A) ((A -> Boolean) (Listof A) -> Boolean)))
(define (one f ls)
  ((true-count 1) f ls))

(: both (All (A) ((A -> Boolean) A A -> Boolean)))
(define (both f a b)
  (and (f a) (f b)))

(: one-or-both (All (A) ((A -> Boolean) A A -> Boolean)))
(define (one-or-both f a b)
  (or (f a) (f b)))

(: neither (All (A) ((A -> Boolean) A A -> Boolean)))
(define (neither f a b)
  (not (one-or-both f a b)))

(: either (All (A) ((A -> Boolean) A A -> Boolean)))
(define (either f a b)
  (not (or (both f a b)
           (neither f a b))))

(: one-or-neither (All (A) ((A -> Boolean) A A -> Boolean)))
(define (one-or-neither f a b)
  (not (both f a b)))
