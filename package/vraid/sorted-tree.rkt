#lang typed/racket

(provide sorted-tree
         make-sorted-tree
         sorted-tree-take-first!
         sorted-tree-add!)

(require vraid/types
         vraid/flow)

(define-type (maybe-tree a)
  (U False (tree a)))

(define-type (sortf a)
  (a a -> Boolean))

(struct: (a) tree
  ([value : a]
   [left : (maybe-tree a)]
   [right : (maybe-tree a)])
  #:mutable)

(struct: (a) sorted-tree
  ([sort : (sortf a)]
   [tree : (maybe-tree a)])
  #:mutable)

(: traverse-right (All (a) ((tree a) -> (tree a))))
(define (traverse-right t)
  (if-let ([right (tree-right t)])
    (traverse-right right)
    t))

(: tree-insert! (All (a) ((sortf a) (tree a) a -> Void)))
(define (tree-insert! sort t item)
  (if-let ([right (tree-right t)])
    (if (sort item (tree-value right))
        (if-let ([left (tree-left t)])
          (if (sort item (tree-value left))
              (set-tree-left! t (tree item #f left))
              (tree-insert! sort left item))
          (set-tree-left! t (tree item #f #f)))
        (tree-insert! sort right item))
    (set-tree-right! t (tree item #f #f))))

(: sorted-tree-add! (All (a) ((sorted-tree a) a -> Void)))
(define (sorted-tree-add! top-tree item)
  (define sort (sorted-tree-sort top-tree))
  (if-let ([t (sorted-tree-tree top-tree)])
    (if (sort item (tree-value t))
        (set-sorted-tree-tree! top-tree (tree item #f t))
        (tree-insert! sort t item))
    (set-sorted-tree-tree! top-tree (tree item #f #f))))

(: sorted-tree-remove-first! (All (a) ((sorted-tree a) -> Void)))
(define (sorted-tree-remove-first! tree)
  (if-let ([t (sorted-tree-tree tree)])
    (if-let ([left (tree-left t)])
      (let ([last (traverse-right left)])
        (begin
          (set-tree-right! last (tree-right t))
          (set-sorted-tree-tree! tree left)))
      (set-sorted-tree-tree! tree (tree-right t)))
    (void)))

(: sorted-tree-take-first! (All (a) ((sorted-tree a) -> (maybe a))))
(define (sorted-tree-take-first! tree)
  (and-let ([t (sorted-tree-tree tree)])
    (let ([value (tree-value t)])
      (begin
        (sorted-tree-remove-first! tree)
        value))))

(: make-sorted-tree (All (a) ((sortf a) -> (sorted-tree a))))
(define (make-sorted-tree sort)
  (sorted-tree sort #f))

(: sorted-tree-first (All (a) ((sorted-tree a) -> (maybe a))))
(define (sorted-tree-first tree)
  (and-let ([t (sorted-tree-tree tree)])
    (tree-value t)))
