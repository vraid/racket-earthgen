#lang typed/racket

(provide generate-rivers!)

(require "planet.rkt"
         "tile-terrain.rkt"
         "corner-terrain.rkt"
         racket/promise
         vraid/flow
         vraid/types
         vraid/sorted-tree)

(: bad-river-end? (planet integer -> Boolean))
(define (bad-river-end? planet n)
  (and (corner-land? planet n)
       (not (corner-river-direction planet n))))

(: has-bad-ends? (planet -> Boolean))
(define (has-bad-ends? planet)
  (ormap (lambda: ([n : integer])
           (bad-river-end? planet n))
         (range (corner-count planet))))

(: lowest-nearby (planet integer -> Flonum))
(define (lowest-nearby planet n)
  (foldl min
         +inf.0
         (map (lambda: ([n : integer])
                (corner-elevation planet n))
              (grid-corner-corner-list planet n))))

(: highest-nearby (planet integer -> Flonum))
(define (highest-nearby planet n)
  (foldl max
         -inf.0
         (map (lambda: ([n : integer])
                (corner-elevation planet n))
              (grid-corner-corner-list planet n))))

(: elevate-ends! (planet -> Void))
(define (elevate-ends! planet)
  (for ([n (corner-count planet)])
    (when (bad-river-end? planet n)
      ((corner-data-elevation-set! (planet-corner planet))
       n
       (* 1.01 (highest-nearby planet n))))))

(define-type corner-node (Pair Integer Flonum))

(: set-directions/floodfill! (planet -> Void))
(define (set-directions/floodfill! planet)
  (let: ([tree : (sorted-tree corner-node) (make-sorted-tree
                                            (lambda: ([a : corner-node]
                                                      [b : corner-node])
                                              (<= (cdr a) (cdr b))))])
    (let*: ([start : integer (argmin (curry tile-elevation planet)
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
                                      ((tile-data-elevation-set! (planet-tile planet)) n elevation)))]
            [check-corner-elevation : (integer Flonum -> Void)
                                    (lambda (n elevation)
                                      (when (< (corner-elevation planet n)
                                               elevation)
                                        ((corner-data-elevation-set! (planet-corner planet)) n (* 1.001 elevation))))]
            [visit/add! : (integer -> Void)
                        (lambda (n)
                          (let ([elevation (corner-elevation planet n)])
                            (for ([k (grid-corner-corner-list planet n)])
                              (unless (corner-visited? k)
                                (corner-visit! k)
                                (check-corner-elevation k elevation)
                                ((corner-data-river-direction-set! (planet-corner planet)) k (grid-corner-corner-position planet k n))
                                (let ([cel (corner-elevation planet k)])
                                  (sorted-tree-add! tree (cons k cel))
                                  (for ([t (grid-corner-tile-list planet k)])
                                    (check-tile-elevation t cel)))))))])
      (letrec: ([recuvisit : (integer -> Void)
                           (lambda (n)
                             (when (tile-water? planet n)
                               (unless (tile-visited? n)
                                 (tile-visit! n)
                                 (begin
                                   (for ([k (grid-tile-corner-list planet n)])
                                     (unless (corner-visited? k)
                                       (when (corner-coast? planet k)
                                         ((corner-data-elevation-set! (planet-corner planet)) k (planet-sea-level planet))
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

(: generate-rivers! (planet -> Void))
(define (generate-rivers! planet)
  (set-directions/floodfill! planet))

(: set-river-directions! (planet -> Void))
(define (set-river-directions! planet)
  (for ([n (corner-count planet)])
    ((corner-data-river-direction-set! (planet-corner planet))
     n
     (let ([lowest (lowest-nearby planet n)])
       (if (< lowest (corner-elevation planet n))
           (lowest-corner-direction planet n)
           -1))))
  (void))

; direction is -1 if no neighbouring corner has lower elevation
(: lowest-corner-direction (planet Integer -> Fixnum))
(define (lowest-corner-direction planet n)
  (let* ([elevation (lambda: ([n : Integer])
                      (corner-elevation planet n))]
         [index/elevation (lambda: ([i : Fixnum])
                            (cons i (elevation
                                     (corner-corner planet n i))))]
         [indices/elevation (map index/elevation
                                 '(0 1 2))])
    (car (argmin (lambda: ([p : fix-float-pair])
                   (cdr p))
                 (cons (cons -1 (elevation n))
                       indices/elevation)))))

(define-type fix-float-pair (Pair Fixnum Flonum))

(: river-sources (planet -> (Integer -> integer-list)))
(define (river-sources planet)
  (let ([ls (build-vector (corner-count planet)
                          (lambda: ([n : Integer])
                            (delay (filter (river-flows-to? planet n)
                                           (grid-corner-corner-list planet n)))))])
    (lambda (n)
      (force (vector-ref ls n)))))
