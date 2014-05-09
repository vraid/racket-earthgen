#lang typed/racket

(provide (all-defined-out)
         (all-from-out "grid.rkt"
                       "planet-typed-data-structs.rkt"
                       "climate-structs.rkt"))

(require "typed-struct-kw.rkt"
         "types.rkt"
         "grid.rkt"
         "planet-typed-data-structs.rkt"
         "climate-structs.rkt"
         math/flonum)

(struct/kw: planet grid
            ([has-climate? : Boolean]
             [climate-parameters : climate-parameters]
             [climate-variables : climate-variables]
             [tile : tile-data]
             [corner : corner-data]
             [edge : edge-data]))

(define tile-count grid-tile-count)
(define corner-count grid-corner-count)
(define edge-count grid-edge-count)

(: edge-tile-sign (planet integer integer -> Flonum))
(define (edge-tile-sign p e t)
  (fl (grid-edge-tile-sign p e t)))

(: edge-corner-sign (planet integer integer -> Flonum))
(define (edge-corner-sign p e c)
  (fl (grid-edge-corner-sign p e c)))

(: tile-coordinates (grid integer -> FlVector))
(define (tile-coordinates p n)
  ((grid-tile-coordinates p) n))

(: corner-coordinates (grid integer -> FlVector))
(define (corner-coordinates p n)
  ((grid-corner-coordinates p) n))

(require (for-syntax racket/syntax
                     racket/list))

(define-syntax (planet-grid-access stx)
  (syntax-case stx ()
    [(_ struct ([field] ...))
     (with-syntax ([(function ...) (map (lambda (field)
                                          (format-id stx "~a-~a" #'struct field))
                                        (syntax->list #'(field ...)))]
                   [(struct-function ...) (map (lambda (field)
                                                 (format-id stx "grid-~a-~a" #'struct field))
                                               (syntax->list #'(field ...)))])
       #'(begin
           (provide function ...)
           (: function (grid integer integer -> integer)) ...
           (define (function p n i)
             ((struct-function p) n i)) ...))]))

(planet-grid-access tile
                    ([tile]
                     [corner]
                     [edge]))

(planet-grid-access corner
                    ([tile]
                     [corner]
                     [edge]))

(planet-grid-access edge
                    ([tile]
                     [corner]))

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
           (: function (id integer -> type)) ...
           (define (function a i)
             ((struct-function (id-alias a)) i)) ...))]))

(direct-access planet tile tile-data
               ([elevation Flonum]
                [water-level Flonum]
                [temperature Flonum]
                [sunlight Flonum]
                [humidity Flonum]
                [precipitation Flonum]
                [snow-cover Flonum]
                [vegetation Flonum]))

(direct-access planet corner corner-data
               ([elevation Flonum]
                [river-direction Fixnum]))

(direct-access planet edge edge-data
               ([has-river? Boolean]
                [river-flow Flonum]))
