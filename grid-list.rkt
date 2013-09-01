#lang lazy

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

(define (n-grid-list n)
  (if (zero? n)
      (grid-list
       (n-grid 0)
       null)
      (let ([grids (n-grid-list (- n 1))])
        (grid-list
         (subdivided-grid (grid-list-first grids))
         grids))))