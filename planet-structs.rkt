#lang typed/racket

(provide (all-defined-out)
         (all-from-out "grid.rkt"
                       "planet-typed-data-structs.rkt"
                       "climate-structs.rkt"))

(require "typed-struct-kw.rkt"
         "types.rkt"
         "grid.rkt"
         "flvector3.rkt"
         "planet-typed-data-structs.rkt"
         "climate-structs.rkt"
         math/flonum)

(struct/kw: planet
  ([grid : grid]
   [has-climate? : Boolean]
   [climate-parameters : climate-parameters]
   [climate-variables : climate-variables]
   [tile : tile-data]
   [corner : corner-data]
   [edge : edge-data]))

(: tile-count (planet -> natural))
(define (tile-count p)
  (grid-tile-count (planet-grid p)))

(: corner-count (planet -> natural))
(define (corner-count p)
  (grid-corner-count (planet-grid p)))

(: edge-count (planet -> natural))
(define (edge-count p)
  (grid-edge-count (planet-grid p)))

(: edge-tile-sign (planet integer integer -> Flonum))
(define (edge-tile-sign p e t)
  (fl (grid-edge-tile-sign (planet-grid p) e t)))

(: edge-corner-sign (planet integer integer -> Flonum))
(define (edge-corner-sign p e c)
  (fl (grid-edge-corner-sign (planet-grid p) e c)))

(require (for-syntax racket/syntax
                     racket/list))

(define-syntax (planet-grid-access stx)
  (syntax-case stx ()
    [(_ id struct ([field] ...))
     (with-syntax ([(function ...) (map (lambda (field)
                                          (format-id stx "~a-~a" #'struct field))
                                        (syntax->list #'(field ...)))]
                   [(struct-function ...) (map (lambda (field)
                                                 (format-id stx "grid-~a-~a" #'struct field))
                                               (syntax->list #'(field ...)))])
       #'(begin
           (provide function ...)
           (: function (id integer integer -> integer)) ...
           (define (function p n i)
             ((struct-function (planet-grid p)) n i)) ...))]))

(planet-grid-access planet tile
                    ([tile]
                     [corner]
                     [edge]))

(planet-grid-access planet corner
                    ([tile]
                     [corner]
                     [edge]))

(planet-grid-access planet edge
                    ([tile]
                     [corner]))

(define-syntax (grid-field-access stx)
  (syntax-case stx ()
    [(_ id struct ([field type] ...))
     (with-syntax ([(function ...) (map (lambda (field)
                                          (format-id stx "~a-~a" #'struct field))
                                        (syntax->list #'(field ...)))]
                   [(struct-function ...) (map (lambda (field)
                                                 (format-id stx "grid-~a-~a" #'struct field))
                                               (syntax->list #'(field ...)))])
       
       #'(begin
           (provide function ...)
           (: function (id integer -> type)) ...
           (define (function p n)
             ((struct-function (planet-grid p)) n)) ...))]))

(grid-field-access planet tile
                   ([coordinates flvector3]))

(grid-field-access planet corner
                   ([coordinates flvector3]))

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
                [vegetation Flonum]))

(direct-access planet corner corner-data
               ([elevation Flonum]
                [river-direction Fixnum]))

(direct-access planet edge edge-data
               ([has-river? Boolean]
                [river-flow Flonum]))
