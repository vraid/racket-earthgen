#lang typed/racket

(provide (struct-out planet)
         tile-count
         corner-count
         edge-count
         (all-from-out "planet-typed-data-structs.rkt"
                       "grid.rkt"))

(require "types.rkt"
         "grid.rkt"
         "vector3.rkt"
         "planet-typed-data-structs.rkt")

(struct: planet
  ([grid : grid]
   [has-climate? : Boolean]
   [tile : tile-data]
   [corner : corner-data]
   [edge : edge-data]))

(: tile-count (planet -> Integer))
(define (tile-count p)
  (grid-tile-count (planet-grid p)))

(: corner-count (planet -> Integer))
(define (corner-count p)
  (grid-corner-count (planet-grid p)))

(: edge-count (planet -> Integer))
(define (edge-count p)
  (grid-edge-count (planet-grid p)))

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
           (: function (id index index -> index)) ...
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
           (: function (id index -> type)) ...
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
