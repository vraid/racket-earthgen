#lang typed/racket

(provide planet/rivers)

(require racket/promise
         vraid/flow
         vraid/types
         vraid/sorted-tree
         "../grid.rkt"
         "../terrain.rkt"
         "../water.rkt"
         "planet-create.rkt")

(: bad-river-end? (planet-water Integer -> Boolean))
(define (bad-river-end? planet n)
  (and (corner-land? planet n)
       (not (corner-river-direction planet n))))

(: has-bad-ends? (planet-water -> Boolean))
(define (has-bad-ends? planet)
  (ormap (lambda ([n : Integer])
           (bad-river-end? planet n))
         (range (corner-count planet))))

(: lowest-nearby (planet-water Integer -> Float))
(define (lowest-nearby planet n)
  (foldl min
         +inf.0
         (map (lambda ([n : Integer])
                (corner-elevation planet n))
              (grid-corner-corner-list planet n))))

(: highest-nearby (planet-terrain Integer -> Float))
(define (highest-nearby planet n)
  (foldl max
         -inf.0
         (map (lambda ([n : Integer])
                (corner-elevation planet n))
              (grid-corner-corner-list planet n))))

(: elevate-ends! (planet-water -> Void))
(define (elevate-ends! planet)
  (for ([n (corner-count planet)])
    (when (bad-river-end? planet n)
      ((corner-terrain-data-elevation-set! (planet-terrain-corner planet))
       n
       (* 1.01 (highest-nearby planet n))))))

(define-type corner-node (Pair Integer Float))

(: set-directions/floodfill! (planet-water -> Void))
(define (set-directions/floodfill! planet)
  (let: ([tree : (sorted-tree corner-node) (make-sorted-tree
                                            (lambda ([a : corner-node]
                                                     [b : corner-node])
                                              (<= (cdr a) (cdr b))))])
    (let* ([start : Integer (argmin (curry tile-elevation planet)
                                    (range (tile-count planet)))]
           [tile-visited : (Vectorof Boolean) (make-vector (tile-count planet) #f)]
           [tile-visited? : (Integer -> Boolean) (lambda (n)
                                                   (vector-ref tile-visited n))]
           [tile-visit! : (Integer -> Void) (lambda (n)
                                              (vector-set! tile-visited n #t))]
           [corner-visited : (Vectorof Boolean) (make-vector (corner-count planet) #f)]
           [corner-visited? : (Integer -> Boolean) (lambda (n)
                                                     (vector-ref corner-visited n))]
           [corner-visit! : (Integer -> Void) (lambda (n)
                                                (vector-set! corner-visited n #t))]
           [coast : (Vectorof Boolean) (make-vector (tile-count planet) #f)]
           [coast? : (Integer -> Boolean) (lambda (n)
                                            (vector-ref coast n))]
           [set-coast! : (Integer -> Void) (lambda (n)
                                             (vector-set! coast n #t))]
           [check-tile-elevation : (Integer Float -> Void)
                                 (lambda (n elevation)
                                   (when (and (not (tile-visited? n))
                                              (< (tile-elevation planet n)
                                                 elevation))
                                     ((tile-terrain-data-elevation-set! (planet-terrain-tile planet)) n elevation)))]
           [check-corner-elevation : (Integer Float -> Void)
                                   (lambda (n elevation)
                                     (when (< (corner-elevation planet n)
                                              elevation)
                                       ((corner-terrain-data-elevation-set! (planet-terrain-corner planet)) n (* 1.001 elevation))))]
           [visit/add! : (Integer -> Void)
                       (lambda (n)
                         (let ([elevation (corner-elevation planet n)])
                           (for ([k (grid-corner-corner-list planet n)])
                             (unless (corner-visited? k)
                               (corner-visit! k)
                               (let ([prev-elevation (corner-elevation planet k)])
                                 (check-corner-elevation k elevation)
                                 ((corner-water-data-river-direction-set! (planet-water-corner planet)) k (grid-corner-corner-position planet k n))
                                 (let ([new-elevation (corner-elevation planet k)])
                                   (sorted-tree-add! tree (cons k prev-elevation))
                                   (for ([t (grid-corner-tile-list planet k)])
                                     (check-tile-elevation t new-elevation))))))))])
      (letrec ([recuvisit : (Integer -> Void)
                          (lambda (n)
                            (when (tile-water? planet n)
                              (unless (tile-visited? n)
                                (tile-visit! n)
                                (begin
                                  (for ([k (grid-tile-corner-list planet n)])
                                    (unless (corner-visited? k)
                                      (when (corner-coast? planet k)
                                        ((corner-terrain-data-elevation-set! (planet-terrain-corner planet)) k (planet-sea-level planet))
                                        (sorted-tree-add! tree (cons k (corner-elevation planet k))))
                                      (corner-visit! k)))
                                  (for ([k (grid-tile-tile-list planet n)])
                                    (recuvisit k))))))]
               [make-next : (-> False)
                          (thunk
                           (and-let* ([val (sorted-tree-take-first! tree)]
                                      [n (car val)])
                                     (begin
                                       (visit/add! n)
                                       (make-next))))])
        
        (recuvisit start)
        (make-next)
        (void)))))

(: river-trees (planet-water -> river-list))
(define (river-trees planet)
  (: corner-node (Integer -> river))
  (define (corner-node n)
    (river n (map corner-node
                  (corner-river-sources planet n))))
  (foldl (lambda ([n : Integer]
                  [ls : river-list])
           (if (corner-coast? planet n)
               (cons (corner-node n) ls)
               ls))
         '()
         (range (corner-count planet))))

(: planet/rivers (planet-water -> planet-water))
(define (planet/rivers p)
  (let* ([tiles (make-tile-water-data (tile-count p))]
         [corners (make-corner-water-data (corner-count p))]
         [p (begin
              ((tile-init p) (tile-water-data-water-level-set! tiles)
                             (lambda ([n : Integer])
                               (planet-sea-level p)))
              ((corner-init p) (corner-water-data-river-direction-set! corners)
                               (lambda ([n : Integer])
                                 -1))
              (planet-water/kw
               #:planet-terrain (copy-planet-geography p)
               #:sea-level (planet-sea-level p)
               #:tile tiles
               #:corner corners
               #:rivers '()))])
    (set-directions/floodfill! p)
    (struct-copy planet-water p
                 [rivers (river-trees p)])))

; direction is -1 if no neighbouring corner has lower elevation
(: lowest-corner-direction (planet-terrain Integer -> Fixnum))
(define (lowest-corner-direction planet n)
  (let* ([elevation (lambda ([n : Integer])
                      (corner-elevation planet n))]
         [index/elevation (lambda ([i : Fixnum])
                            (cons i (elevation
                                     (corner-corner planet n i))))]
         [indices/elevation (map index/elevation
                                 '(0 1 2))])
    (car (argmin (lambda ([p : fix-float-pair])
                   (cdr p))
                 (cons (cons -1 (elevation n))
                       indices/elevation)))))

(define-type fix-float-pair (Pair Fixnum Float))
