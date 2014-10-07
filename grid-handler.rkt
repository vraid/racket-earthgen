#lang racket

(provide new-grid-handler)

(require "grid-list.rkt")

(define (new-grid-handler)
  (new grid-handler%))
  
(define grid-handler%
  (class object%
    (super-new)
    (define grids (n-grid-list '() 0))
    (define/public (get-grids size)
      (set! grids (n-grid-list grids size))
      grids)
    (define/public (get-grid size)
      (first (get-grids size)))))
