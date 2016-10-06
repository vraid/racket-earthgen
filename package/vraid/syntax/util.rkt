#lang racket

(provide (for-syntax syntax->keyword))

(begin-for-syntax
  (define (syntax->keyword stx)
    (string->keyword (symbol->string (syntax->datum stx)))))
