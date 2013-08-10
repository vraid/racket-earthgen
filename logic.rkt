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

(define true? (lambda (a) (eq? true a)))
(define false? (lambda (a) (not (true? a))))

(define all andmap)
(define any ormap)
(define true-count (lambda (n) (lambda (f ls) (= n (stream-count f ls)))))
(define one (true-count 1))
(define none (lambda (f ls) (not (any f ls))))

(define both (lambda (f a b) (and (f a) (f b))))
(define neither (lambda (f a b) (not (or (f a) (f b)))))
(define either (lambda (f a b) (not (or (both f a b) (neither f a b)))))
(define one-or-both (lambda (f a b) (or (f a) (f b))))