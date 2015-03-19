#lang typed/racket

(provide generate-rivers)

(require racket/promise
         vraid/flow
         vraid/types
         vraid/sorted-tree
         "../grid.rkt"
         "../terrain.rkt"
         "planet-create.rkt")

(: bad-river-end? (planet-terrain integer -> Boolean))
(define (bad-river-end? planet n)
  (and (corner-land? planet n)
       (not (corner-river-direction planet n))))

(: has-bad-ends? (planet-terrain -> Boolean))
(define (has-bad-ends? planet)
  (ormap (lambda ([n : integer])
           (bad-river-end? planet n))
         (range (corner-count planet))))

(: lowest-nearby (planet-terrain integer -> Flonum))
(define (lowest-nearby planet n)
  (foldl min
         +inf.0
         (map (lambda ([n : integer])
                (corner-elevation planet n))
              (grid-corner-corner-list planet n))))

(: highest-nearby (planet-terrain integer -> Flonum))
(define (highest-nearby planet n)
  (foldl max
         -inf.0
         (map (lambda ([n : integer])
                (corner-elevation planet n))
              (grid-corner-corner-list planet n))))

(: elevate-ends! (planet-terrain -> Void))
(define (elevate-ends! planet)
  (for ([n (corner-count planet)])
    (when (bad-river-end? planet n)
      ((corner-terrain-data-elevation-set! (planet-terrain-corner planet))
       n
       (* 1.01 (highest-nearby planet n))))))

(define-type corner-node (Pair Integer Flonum))

(: set-directions/floodfill! (planet-terrain -> Void))
(define (set-directions/floodfill! planet)
  (let: ([tree : (sorted-tree corner-node) (make-sorted-tree
                                            (lambda ([a : corner-node]
                                                     [b : corner-node])
                                              (<= (cdr a) (cdr b))))])
    (let* ([start : integer (argmin (curry tile-elevation planet)
                                    (range (tile-count planet)))]
           [tile-visited : (Vectorof Boolean) (make-vector (tile-count planet) #f)]
           [tile-visited? : (integer -> Boolean) (lambda (n)
                                                   (vector-ref tile-visited n))]
           [tile-visit! : (integer -> Void) (lambda (n)
                                              (vector-set! tile-visited n #t))]
           [corner-visited : (Vectorof Boolean) (make-vector (corner-count planet) #f)]
           [corner-visited? : (integer -> Boolean) (lambda (n)
                                                     (vector-ref corner-visited n))]
           [corner-visit! : (integer -> Void) (lambda (n)
                                                (vector-set! corner-visited n #t))]
           [coast : (Vectorof Boolean) (make-vector (tile-count planet) #f)]
           [coast? : (integer -> Boolean) (lambda (n)
                                            (vector-ref coast n))]
           [set-coast! : (integer -> Void) (lambda (n)
                                             (vector-set! coast n #t))]
           [check-tile-elevation : (integer Flonum -> Void)
                                 (lambda (n elevation)
                                   (when (and (not (tile-visited? n))
                                              (< (tile-elevation planet n)
                                                 elevation))
                                     ((tile-terrain-data-elevation-set! (planet-terrain-tile planet)) n elevation)))]
           [check-corner-elevation : (integer Flonum -> Void)
                                   (lambda (n elevation)
                                     (when (< (corner-elevation planet n)
                                              elevation)
                                       ((corner-terrain-data-elevation-set! (planet-terrain-corner planet)) n (* 1.001 elevation))))]
           [visit/add! : (integer -> Void)
                       (lambda (n)
                         (let ([elevation (corner-elevation planet n)])
                           (for ([k (grid-corner-corner-list planet n)])
                             (unless (corner-visited? k)
                               (corner-visit! k)
                               (let ([prev-elevation (corner-elevation planet k)])
                                 (check-corner-elevation k elevation)
                                 ((corner-terrain-data-river-direction-set! (planet-terrain-corner planet)) k (grid-corner-corner-position planet k n))
                                 (let ([new-elevation (corner-elevation planet k)])
                                   (sorted-tree-add! tree (cons k prev-elevation))
                                   (for ([t (grid-corner-tile-list planet k)])
                                     (check-tile-elevation t new-elevation))))))))])
      (letrec ([recuvisit : (integer -> Void)
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

(: river-trees (planet-terrain -> river-list))
(define (river-trees planet)
  (: corner-node (integer -> river))
  (define (corner-node n)
    (river n (map corner-node
                  (corner-river-sources planet n))))
  (foldl (lambda ([n : integer]
                  [ls : river-list])
           (if (corner-coast? planet n)
               (cons (corner-node n) ls)
               ls))
         '()
         (range (corner-count planet))))

(: generate-rivers (planet-terrain -> planet-terrain))
(define (generate-rivers planet)
  (let ([p (copy-planet-geography planet)])
    (set-directions/floodfill! p)
    (planet-terrain/kw
     #:planet-geometry p
     #:sea-level (planet-sea-level p)
     #:tile (planet-terrain-tile p)
     #:corner (planet-terrain-corner p)
     #:rivers (river-trees p))))

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

(define-type fix-float-pair (Pair Fixnum Flonum))
