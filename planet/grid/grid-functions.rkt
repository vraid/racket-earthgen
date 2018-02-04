#lang typed/racket

(require vraid/types
         math/flonum
         "grid-structs.rkt")

(provide (all-defined-out))

(: subdivision-level-tile-count (Natural -> Integer))
(define (subdivision-level-tile-count n)
  (+ 2 (* 10 (expt 3 n))))

(: subdivision-level-corner-count (Natural -> Integer))
(define (subdivision-level-corner-count n)
  (* 20 (expt 3 n)))

(: subdivision-level-edge-count (Natural -> Integer))
(define (subdivision-level-edge-count n)
  (* 30 (expt 3 n)))

(: grid-tile-count (grid -> Integer))
(define (grid-tile-count grid)
  (subdivision-level-tile-count (grid-subdivision-level grid)))

(: grid-corner-count (grid -> Integer))
(define (grid-corner-count grid)
  (subdivision-level-corner-count (grid-subdivision-level grid)))

(: grid-edge-count (grid -> Integer))
(define (grid-edge-count grid)
  (subdivision-level-edge-count (grid-subdivision-level grid)))

(: tile-edge-count (Integer -> Integer))
(define (tile-edge-count t)
  (if (> 12 t) 5 6))

(: corner-edge-count Natural)
(define corner-edge-count 3)

(: grid-edge-tile-sign (grid Integer Integer -> (U 0 1 -1)))
(define (grid-edge-tile-sign g e t)
  (cond [(= t ((grid-edge-tile g) e 0)) 1]
        [(= t ((grid-edge-tile g) e 1)) -1]
        [else 0]))

(: grid-edge-corner-sign (grid Integer Integer -> (U 0 1 -1)))
(define (grid-edge-corner-sign g e c)
  (cond [(= c ((grid-edge-corner g) e 0)) 1]
        [(= c ((grid-edge-corner g) e 1)) -1]
        [else 0]))

(: grid-access-position ((grid -> get-grid-integer) (Integer -> Integer) grid Integer Integer -> Integer))
(define (grid-access-position f count g n i)
  (let ([n-count (count n)]
        [f-g (f g)])
    (: iterate (Integer -> Integer))
    (define (iterate k)
      (cond [(= k n-count) -1]
            [(= i (f-g n k)) k]
            [else (iterate (+ 1 k))]))
    (iterate 0)))

(: grid-access-list ((grid -> get-grid-integer) (Integer -> Integer) grid Integer -> integer-list))
(define (grid-access-list f count g n)
  (map (curry (f g) n) (range (count n))))

(: grid-access-set ((grid -> get-grid-integer) (Integer -> Integer) grid Integer -> integer-set))
(define (grid-access-set f count g n)
  (list->set (grid-access-list f count g n)))

(: grid-access-vector ((grid -> get-grid-integer) (Integer -> Integer) grid Integer -> integer-vector))
(define (grid-access-vector f count g n)
  (build-vector (count n) (curry (f g) n)))

(require (for-syntax racket/syntax))

(define-syntax (grid-access stx)
  (syntax-case stx ()
    [(_ id count ([field] ...))
     (let* ([field-access (λ (f)
                            (format-id stx "grid-~a-~a" #'id f))]
            [fields (syntax->list #'(field ...))]
            [access-type (λ (pattern)
                           (λ (field)
                             (format-id stx pattern (field-access field))))])
       (with-syntax ([(access ...) (map field-access fields)]
                     [(access-list ...) (map (access-type "~a-list") fields)]
                     [(access-set ...) (map (access-type "~a-set") fields)]
                     [(access-vector ...) (map (access-type "~a-vector") fields)]
                     [(access-position ...) (map (access-type "~a-position") fields)])
         #'(begin
             (: access-list (grid Integer -> integer-list)) ...
             (define (access-list g n)
               (grid-access-list access count g n)) ...
             (: access-set (grid Integer -> integer-set)) ...
             (define (access-set g n)
               (grid-access-set access count g n)) ...
             (: access-vector (grid Integer -> integer-vector)) ...
             (define (access-vector g n)
               (grid-access-vector access count g n)) ...
             (: access-position (grid Integer Integer -> Integer)) ...
             (define (access-position g n i)
               (grid-access-position access count g n i)) ...)))]))

(grid-access tile tile-edge-count
             ([tile] [corner] [edge]))

(grid-access corner (λ ([n : Integer]) corner-edge-count)
             ([tile] [corner] [edge]))

(grid-access edge (λ ([n : Integer]) 2)
             ([tile] [corner]))

(require (for-syntax racket/syntax))

(define-syntax (planet-grid-access stx)
  (syntax-case stx ()
    [(_ struct ([field] ...))
     (with-syntax ([(function ...) (map (λ (field)
                                          (format-id stx "~a-~a" #'struct field))
                                        (syntax->list #'(field ...)))]
                   [(struct-function ...) (map (λ (field)
                                                 (format-id stx "grid-~a-~a" #'struct field))
                                               (syntax->list #'(field ...)))])
       #'(begin
           (provide function ...)
           (: function (grid Integer Integer -> Integer)) ...
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

(define tile-count grid-tile-count)
(define corner-count grid-corner-count)
(define edge-count grid-edge-count)

(: edge-tile-sign (grid Integer Integer -> Float))
(define (edge-tile-sign p e t)
  (fl (grid-edge-tile-sign p e t)))

(: edge-corner-sign (grid Integer Integer -> Float))
(define (edge-corner-sign p e c)
  (fl (grid-edge-corner-sign p e c)))

(: tile-coordinates (grid Integer -> FlVector))
(define (tile-coordinates p n)
  ((grid-tile-coordinates p) n))

(: corner-coordinates (grid Integer -> FlVector))
(define (corner-coordinates p n)
  ((grid-corner-coordinates p) n))
