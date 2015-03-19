#lang typed/racket

(provide direct-access)

(require vraid/types
         (for-syntax racket/syntax))

(define-syntax (direct-access stx)
  (syntax-case stx ()
    [(_ id alias struct ([field type] ...))
     (with-syntax ([(function ...) (map (lambda (field)
                                          (format-id stx "~a-~a" #'alias field))
                                        (syntax->list #'(field ...)))]
                   [(struct-function ...) (map (lambda (field)
                                                 (format-id stx "~a-~a" #'struct field))
                                               (syntax->list #'(field ...)))]
                   [id-alias (format-id stx "~a-~a" #'id #'alias)])
       
       #'(begin
           (provide function ...)
           (: function (id integer -> type)) ...
           (define (function a i)
             ((struct-function (id-alias a)) i)) ...))]))
