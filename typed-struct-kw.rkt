#lang typed/racket

(require (for-syntax racket/syntax
                     racket/list))

(provide struct/kw:)

(begin-for-syntax
  (define (syntax->keyword stx)
    (string->keyword (symbol->string (syntax->datum stx)))))

(define-syntax (struct/kw: stx)
  (syntax-case stx ()
    [(_ id ([field : type] ...) opt ...)
     (with-syntax ([kw-ctor (format-id stx "~a/kw" #'id)]
                   [(kw+type ...) (append*
                                   (map (lambda (fld type)
                                          (list (syntax->keyword fld)
                                                type))
                                        (syntax->list #'(field ...))
                                        (syntax->list #'(type ...))))]
                   [(kw+fld ...) (append*
                                  (map (lambda (fld)
                                         (list (syntax->keyword fld)
                                               fld))
                                       (syntax->list #'(field ...))))])
       #'(begin
           (struct: id ([field : type] ...)
             opt ...)
           (define kw-ctor
             (ann (lambda (kw+fld ...)
                    (id field ...))
                  (kw+type ... -> id)))))]))
