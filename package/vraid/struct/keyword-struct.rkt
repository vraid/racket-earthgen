#lang typed/racket

(require (for-syntax racket/syntax
                     racket/list
                     racket/struct-info
                     syntax/parse)
         "../syntax/util.rkt")

(provide struct/kw)

(define-syntax struct/kw
  (syntax-parser
    [(_ id (~optional super-id:id) ([field : type] ...) opt ...)
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
       (if (not (attribute super-id))
           #'(begin
               (struct: id ([field : type] ...)
                 opt ...)
               (define kw-ctor
                 (ann (lambda (kw+fld ...)
                        (id field ...))
                      (kw+type ... -> id))))
           (with-syntax
               ([super-kw (syntax->keyword #'super-id)]
                [(super-accessors ...) (reverse (cadddr (extract-struct-info (syntax-local-value #'super-id))))])
             #'(begin
                 (struct: id super-id ([field : type] ...)
                   opt ...)
                 (define kw-ctor
                   (ann (lambda (super-kw super kw+fld ...)
                          (id (super-accessors super) ...
                              field ...))
                        (super-kw super-id kw+type ... -> id)))))))]))

