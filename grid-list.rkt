#lang lazy

(require "grid-structs.rkt")
(require "grid-functions.rkt")
(require "grid-create.rkt")

(provide grid-list?)
(provide grid-list-first)
(provide grid-list-rest)
(provide make-grid-list)
(provide n-grid-list)

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