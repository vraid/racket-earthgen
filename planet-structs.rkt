#lang typed/racket

(provide (struct-out planet)
         (all-from-out "planet-typed-data-structs.rkt"
                       "grid.rkt"))

(require "types.rkt"
         "grid.rkt"
         "planet-typed-data-structs.rkt")

(struct: planet
  ([grid : grid]
   [tile : tile-data]
   [corner : corner-data]
   [edge : edge-data]))

(require (for-syntax racket/syntax
                     racket/list))

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
           (: function (id index -> type)) ...
           (define (function a i)
             ((struct-function (id-alias a)) i)) ...))]))

(direct-access planet tile tile-data
               ([elevation Flonum]
                [water-level Flonum]
                [temperature Flonum]
                [humidity Flonum]
                [precipitation Flonum]))

(direct-access planet corner corner-data
               ([elevation Flonum]
                [river-direction Fixnum]))

(direct-access planet edge edge-data
               ([has-river? Boolean]
                [river-flow Flonum]))
