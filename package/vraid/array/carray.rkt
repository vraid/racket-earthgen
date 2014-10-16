#lang racket

(provide struct-array
         make-int-array)

(require ffi/unsafe)
(require (for-syntax racket/syntax))

(define (make-c-array type length)
  (let* ([arr-type (_array type length)]
         [arr (malloc arr-type)]
         [ref (lambda (n)
                (ptr-ref arr type n))]
         [set! (lambda (n k)
                 (ptr-set! arr type n k))])
    (values ref set!)))

(define (make-int-array length)
  (make-c-array _int length))

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
                    [make-array (format-id stx "make-~a" #'id)])
                   #'(begin
                       (define-cstruct cstruct-id
                         ([field type] ...))
                       (struct id
                         (field ... set-field! ...))
                       (define (make-array n)
                         (let* ([arr-type (_array cstruct-id n)]
                                [arr (let ([a (malloc arr-type)])
                                       (memset a 0 n cstruct-id) a)]
                                [ref (lambda (i)
                                       (ptr-ref arr cstruct-id i))])
                           (id
                            (lambda (i) (c-field (ref i))) ...
                            (lambda (i a) (set-cfield! (ref i) a)) ...)))))]))
