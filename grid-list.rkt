#lang racket

(require "grid-structs.rkt"
         "grid-functions.rkt"
         "grid-create.rkt")

(provide grid-list?
         grid-list-first
         grid-list-rest
         make-grid-list
         n-grid-list)

(struct grid-list
  (first
   rest)
  #:transparent)

(define (make-grid-list grid)
  (let* ([level (grid-subdivision-level grid)]
         [rest (if (zero? level)
                   null
                   (n-grid-list (- level 1)))])
    (grid-list
     grid
     rest)))

(define (n-grid-list grids n)
  (if (zero? n)
      (grid-list
       (n-grid 0)
       null)
      (if (eq? n (grid-subdivision-level (grid-list-first grids)))
          grids
          (if (< n (grid-subdivision-level (grid-list-first grids)))
              (n-grid-list (grid-list-rest grids) n)
              (n-grid-list (grid-list
                            (subdivided-grid (grid-list-first grids))
                            grids)
                           n)))))