#lang racket

(provide first-member)

(define first-member
  (lambda (a ls)
    (first (member a ls))))