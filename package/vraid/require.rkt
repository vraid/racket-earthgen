#lang racket

(provide require/provide)

(require (for-syntax racket/syntax))

(define-syntax (require/provide stx)
  (syntax-case stx ()
    [(_ paths ...)
     #'(begin
         (require paths ...)
         (provide (all-from-out paths ...)))]))
