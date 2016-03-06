#lang racket

(provide (all-defined-out))

(define-syntax (if-not stx)
  (syntax-case stx ()
    [(_ condition then else)
     #'(if condition else then)]))

(define-syntax (if-let stx)
  (syntax-case stx ()
    [(_ ((binding value) ...) then else)
     #'(let ([binding value] ...)
         (if (and binding ...)
             then
             else))]))

(define-syntax (and-let stx)
  (syntax-case stx ()
    [(_ ((binding value) ...) then)
     #'(if-let ((binding value) ...) then #f)]))

(define-syntax (if-let* stx)
  (syntax-case stx ()
    [(_ () then else)
     #'then]
    [(_ ((binding value) bindings ...) then else)
     #'(let ([binding value])
         (if binding
             (if-let* (bindings ...) then else)
             else))]))

(define-syntax (and-let* stx)
  (syntax-case stx ()
    [(_ (bindings ...) expression)
     #'(if-let* (bindings ...) expression #f)]))

(define-syntax (when-let stx)
  (syntax-case stx ()
    [(_ ((binding value) ...) then)
     #'(if-let ((binding value) ...) then (void))]))

(define-syntax (when-let* stx)
  (syntax-case stx ()
    [(_ (bindings ...) expression)
     #'(if-let* (bindings ...) expression (void))]))
