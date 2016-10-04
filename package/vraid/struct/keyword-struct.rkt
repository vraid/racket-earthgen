#lang typed/racket

(require (for-syntax racket/syntax
                     racket/list
                     racket/struct-info))

(provide struct/kw)

(begin-for-syntax
  (define (syntax->keyword stx)
    (string->keyword (symbol->string (syntax->datum stx)))))

(define-syntax (struct/kw stx)
  (syntax-case stx ()
    [(_ id ([field : type] ...) opt ...)
     (with-syntax ([kw-ctor (format-id #'id "~a/kw" #'id)]
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
                  (kw+type ... -> id)))))]
    [(_ id super-id ([field : type] ...) opt ...)
     (with-syntax ([kw-ctor (format-id #'id "~a/kw" #'id)]
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
                                       (syntax->list #'(field ...))))]
                   [super-kw (syntax->keyword #'super-id)]
                   [(super-accessors ...) (reverse (cadddr (extract-struct-info (syntax-local-value #'super-id))))])
       #'(begin
           (struct: id super-id ([field : type] ...)
             opt ...)
           (define kw-ctor
             (ann (lambda (super-kw super kw+fld ...)
                    (id (super-accessors super) ...
                        field ...))
                  (super-kw super-id kw+type ... -> id)))))]))
