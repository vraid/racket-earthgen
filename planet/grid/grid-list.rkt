#lang typed/racket

(require "grid-structs.rkt"
         "grid-create.rkt")

(provide grid-list
         0-grid-list
         n-grid-list)

(define-type grid-list (Listof grid))

(define 0-grid-list (list (n-grid 0)))

(: n-grid-list (grid-list Integer -> grid-list))
(define (n-grid-list grids n)
  (cond [(zero? n) 0-grid-list]
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
