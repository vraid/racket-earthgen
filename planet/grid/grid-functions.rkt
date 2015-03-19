#lang typed/racket

(require vraid/types
         vraid/math
         vraid/util
         math/flonum
         "grid-structs.rkt")

(provide (all-defined-out))

(: subdivision-level-tile-count (natural -> integer))
(define (subdivision-level-tile-count n)
  (+ 2 (* 10 (expt 3 n))))

(: subdivision-level-corner-count (natural -> integer))
(define (subdivision-level-corner-count n)
  (* 20 (expt 3 n)))

(: subdivision-level-edge-count (natural -> integer))
(define (subdivision-level-edge-count n)
  (* 30 (expt 3 n)))

(: grid-tile-count (grid -> integer))
(define (grid-tile-count grid)
  (subdivision-level-tile-count (grid-subdivision-level grid)))

(: grid-corner-count (grid -> integer))
(define (grid-corner-count grid)
  (subdivision-level-corner-count (grid-subdivision-level grid)))

(: grid-edge-count (grid -> integer))
(define (grid-edge-count grid)
  (subdivision-level-edge-count (grid-subdivision-level grid)))

(: tile-edge-count (integer -> integer))
(define (tile-edge-count t)
  (if (> 12 t) 5 6))

(: corner-edge-count natural)
(define corner-edge-count 3)

(: grid-edge-tile-sign (grid integer integer -> (U 0 1 -1)))
(define (grid-edge-tile-sign g e t)
  (cond [(eq? t ((grid-edge-tile g) e 0)) 1]
        [(eq? t ((grid-edge-tile g) e 1)) -1]
        [else 0]))

(: grid-edge-corner-sign (grid integer integer -> (U 0 1 -1)))
(define (grid-edge-corner-sign g e c)
  (cond [(eq? c ((grid-edge-corner g) e 0)) 1]
        [(eq? c ((grid-edge-corner g) e 1)) -1]
        [else 0]))

(: grid-access-position ((grid -> get-grid-integer) (integer -> integer) grid integer integer -> Integer))
(define (grid-access-position f count g n i)
  (let ([n-count (count n)]
        [f-g (f g)])
    (: iterate (integer -> Integer))
    (define (iterate k)
      (if (= k n-count)
          -1
          (if (= i (f-g n k))
              k
              (iterate (+ 1 k)))))
    (iterate 0)))

(: grid-access-list ((grid -> get-grid-integer) (integer -> integer) grid integer -> integer-list))
(define (grid-access-list f count g n)
  (map (curry (f g) n) (range (count n))))

(: grid-access-set ((grid -> get-grid-integer) (integer -> integer) grid integer -> integer-set))
(define (grid-access-set f count g n)
  (list->set (grid-access-list f count g n)))

(: grid-access-vector ((grid -> get-grid-integer) (integer -> integer) grid integer -> integer-vector))
(define (grid-access-vector f count g n)
  (build-vector (count n) (curry (f g) n)))

(require (for-syntax racket/syntax))

(define-syntax (grid-access stx)
  (syntax-case stx ()
    [(_ id count ([field] ...))
     (let* ([field-access (lambda (f)
                            (format-id stx "grid-~a-~a" #'id f))]
            [fields (syntax->list #'(field ...))]
            [access-type (lambda (pattern)
                           (lambda (field)
                             (format-id stx pattern (field-access field))))])
       (with-syntax ([(access ...) (map field-access fields)]
                     [(access-list ...) (map (access-type "~a-list") fields)]
                     [(access-set ...) (map (access-type "~a-set") fields)]
                     [(access-vector ...) (map (access-type "~a-vector") fields)]
                     [(access-position ...) (map (access-type "~a-position") fields)])
         #'(begin
             (: access-list (grid integer -> integer-list)) ...
             (define (access-list g n)
               (grid-access-list access count g n)) ...
             (: access-set (grid integer -> integer-set)) ...
             (define (access-set g n)
               (grid-access-set access count g n)) ...
             (: access-vector (grid integer -> integer-vector)) ...
             (define (access-vector g n)
               (grid-access-vector access count g n)) ...
             (: access-position (grid integer integer -> Integer)) ...
             (define (access-position g n i)
               (grid-access-position access count g n i)) ...)))]))

(grid-access tile tile-edge-count
             ([tile] [corner] [edge]))

(grid-access corner (lambda: ([n : integer]) corner-edge-count)
             ([tile] [corner] [edge]))

(grid-access edge (lambda: ([n : integer]) 2)
             ([tile] [corner]))

(require (for-syntax racket/syntax))

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

(define tile-count grid-tile-count)
(define corner-count grid-corner-count)
(define edge-count grid-edge-count)

(: edge-tile-sign (grid integer integer -> flonum))
(define (edge-tile-sign p e t)
  (fl (grid-edge-tile-sign p e t)))

(: edge-corner-sign (grid integer integer -> flonum))
(define (edge-corner-sign p e c)
  (fl (grid-edge-corner-sign p e c)))

(: tile-coordinates (grid integer -> flvector3))
(define (tile-coordinates p n)
  ((grid-tile-coordinates p) n))

(: corner-coordinates (grid integer -> flvector3))
(define (corner-coordinates p n)
  ((grid-corner-coordinates p) n))
