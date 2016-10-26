#lang typed/racket

(require typed/racket/class
         "../planet/grid-base.rkt"
         "../planet/grid-create.rkt")

(provide new-grid-handler)

(define-type grid-handler-class (Class [get-grids (Integer -> grid-list)]
                                       [get-grid (Integer -> grid)]))

(: new-grid-handler (-> (Instance grid-handler-class)))
(define (new-grid-handler)
  (new grid-handler%))

(: grid-handler% grid-handler-class)
(define grid-handler%
  (class object%
    (super-new)
    (define grids (n-grid-list '() 0))
    (define/public (get-grids size)
      (set! grids (n-grid-list grids size))
      grids)
    (define/public (get-grid size)
      (first (get-grids size)))))
