#lang typed/racket

(provide (struct-out map-mode)
         define-map-modes)

(require vraid/color
         vraid/flow
         "planet/grid.rkt")

(struct map-mode
  ([name : Symbol]
   [condition : (grid -> Boolean)]
   [function : (grid Integer -> flcolor)])
  #:transparent)

(define color-undefined
  (flcolor3 0.7 0.7 0.7))

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
