#lang racket

(provide if-let
         and-let)

(define-syntax (if-let stx)
  (syntax-case stx ()
    [(_ ((binding value) ...) then else)
     #'(let ((binding value) ...)
         (if (and binding ...)
             then
             else))]))

(define-syntax (and-let stx)
  (syntax-case stx ()
    [(_ ((binding value) ...) expression)
     #'(if-let ((binding value) ...)
               expression
               #f)]))
