#lang typed/racket

(require (for-syntax racket/syntax
                     racket/list)
         "../syntax/util.rkt")

(require racket/flonum
         "keyword-struct.rkt"
         "../util/vector-util.rkt")

(provide vector-struct)

(define-syntax (vector-struct stx)
  (syntax-case stx ()
    [(_ id ([field : type] ...) opt ...)
     (with-syntax ([(set ...) (map (lambda (field)
                                     (format-id field "~a-set!" field))
                                   (syntax->list #'(field ...)))]
                   [struct/accessors (format-id #'id "~a/accessors" #'id)]
                   [(kw+vector-type ...) (append*
                                          (map (lambda (fld type)
                                                 (list (syntax->keyword fld)
                                                       (list #'vector-accessor type)))
                                               (syntax->list #'(field ...))
                                               (syntax->list #'(type ...))))]
                   [(kw+fld ...) (append*
                                  (map (lambda (fld)
                                         (list (syntax->keyword fld)
                                               fld))
                                       (syntax->list #'(field ...))))])
       
       #'(begin
           (struct/kw id
                      ([field : (Integer -> type)] ...
                       [set : (Integer type -> Void)] ...) opt ...)
           (define struct/accessors
             (ann (lambda (kw+fld ...)
                    (id (vector-accessor-get field) ...
                        (vector-accessor-set field) ...))
                  (kw+vector-type ... -> id)))))]))
