#lang typed/racket

(require vraid/types
         "grid-structs.rkt"
         "grid-functions.rkt"
         "grid-create.rkt")

(provide grid-list
         n-grid-list)

(define-type grid-list (Listof grid))

(: n-grid-list (grid-list integer -> grid-list))
(define (n-grid-list grids n)
  (cond [(zero? n)
         (list (n-grid 0))]
        [(null? grids)
         (n-grid-list
          (n-grid-list null (- n 1))
          n)]
        [(eq? n (grid-subdivision-level (first grids)))
         grids]
        [(< n (grid-subdivision-level (first grids)))
         (n-grid-list (rest grids) n)]
        [else (n-grid-list (cons (subdivided-grid (first grids))
                                 grids)
                           n)]))
