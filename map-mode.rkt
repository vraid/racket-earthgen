#lang typed/racket

(provide (all-defined-out))

(require vraid/color
         "color-base.rkt"
         "planet/grid-base.rkt")

(struct map-mode
  ([name : Symbol]
   [condition : (grid -> Boolean)]
   [function : (grid Integer -> flcolor)])
  #:transparent)

(require (for-syntax racket/syntax))

(define-syntax (define-map-modes stx)
  (syntax-case stx ()
    [(_ category condition (name function) ...)
     (with-syntax ([category-definition (format-id stx "~a-map-modes" #'category)]
                   [(definition ...) (map (lambda (name)
                                            (format-id stx "~a-map-mode" name))
                                          (syntax->list #'(name ...)))])
       #'(begin
           (define definition
             (map-mode 'name
                       condition
                       (lambda ([p : grid]
                                      [n : Integer])
                               (if (condition p)
                                   (function p n)
                                   color-undefined)))) ...
           (define category-definition
             (list definition ...))))]))
