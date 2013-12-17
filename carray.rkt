#lang racket

(provide struct-array)

(require ffi/unsafe)
(require (for-syntax racket/syntax
                     racket/list))

(define-syntax (struct-array stx)
  (syntax-case stx ()
    [(_ id ([field type] ...))
     (with-syntax* ([c-id (format-id stx "c~a" #'id)]
                    [cstruct-id (format-id stx "_c~a" #'id)]
                    [(c-field ...) (map (lambda (field)
                                          (format-id stx "~a-~a" #'c-id field))
                                        (syntax->list #'(field ...)))]
                    [(set-cfield! ...) (map (lambda (field)
                                              (format-id stx "set-~a-~a!" #'c-id field))
                                            (syntax->list #'(field ...)))]
                    [(set-field! ...) (map (lambda (field)
                                             (format-id stx "~a-set!" field))
                                           (syntax->list #'(field ...)))]
                    [array-id (format-id stx "~a-array" #'id)]
                    [module-id (format-id stx "m~a" #'id)]
                    [make-array (format-id stx "make-~a" #'id)])
                   
                   #'(begin
                       (define-cstruct cstruct-id
                         ([field type] ...))
                       (struct id
                         (field ... set-field! ...))
                       (define (make-array n)
                         (let* ([arr-type (_array cstruct-id n)]
                                [arr (malloc arr-type)]
                                [ref (lambda (i)
                                       (ptr-ref arr cstruct-id i))])
                           (id
                            (lambda (i) (c-field (ref i))) ...
                            (lambda (i a) (set-cfield! (ref i) a)) ...)))))]))
