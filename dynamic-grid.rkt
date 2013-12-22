#lang typed/racket

(provide (all-defined-out)
         (all-from-out "dynamic-grid-structs.rkt"
                       "dynamic-grid-access.rkt"))

(require "dynamic-grid-structs.rkt"
         "dynamic-grid-access.rkt"
         "vector3.rkt"
         "color.rkt"
         "typed-logic.rkt"
         math/flonum)

(require/typed "image-overlay.rkt"
               (coord->color (flvector3 -> flcolor)))

(define-type grid-constraint (flvector3 -> Boolean))

(define icosahedron-coordinates
  (let* ([x 0.525731112119133606]
         [z 0.850650808352039932]
         [-x (- x)]
         [-z (- z)])
    (vector
     (flvector x 0.0 -z)
     (flvector -x 0.0 -z)
     (flvector x 0.0 z)
     (flvector -x 0.0 z)
     (flvector 0.0 -z -x)
     (flvector 0.0 -z x)
     (flvector 0.0 z -x)
     (flvector 0.0 z x)
     (flvector -z -x 0.0)
     (flvector z -x 0.0)
     (flvector -z x 0.0)
     (flvector z x 0.0))))

(: icosahedron-tiles (Vectorof (Vectorof Integer)))
(define icosahedron-tiles
  (vector
   (vector 1 6 11 9 4)
   (vector 0 4 8 10 6)
   (vector 3 5 9 11 7)
   (vector 2 7 10 8 5)
   (vector 0 9 5 8 1)
   (vector 2 3 8 4 9)
   (vector 0 1 10 7 11)
   (vector 2 11 6 10 3)
   (vector 1 4 5 3 10)
   (vector 0 11 2 5 4)
   (vector 1 8 3 7 6)
   (vector 0 6 7 2 9)))

(: connect-corners (tile corner corner -> Void))
(define (connect-corners t a b)
  (begin
    (unless (empty-corner? a)
      (let ([i (corner-tile-index a t)])
        (vector-set! (corner-corners a) i
                     b))))
  (unless (empty-corner? b)
    (let ([i (corner-tile-index b t)])
      (vector-set! (corner-corners b) (corner-index b (+ i 1))
                   a))))

(: add-corner (tile tile tile -> corner))
(define (add-corner a b c)
  (let ([n (corner
            (flvector3-normal
             (apply flvector3+ (map (lambda: ([t : tile])
                                      (tile-coordinates t))
                                    (list a b c))))
            (vector a b c)
            (make-vector 3 empty-corner))])
    (begin
      (: connect (tile corner -> Void))
      (define (connect t c)
        (connect-corners t c (tile-corner t (+ 1 (tile-corner-index t c)))))
      
      (vector-set! (tile-corners a) (tile-tile-index a c) n)
      (vector-set! (tile-corners b) (tile-tile-index b a) n)
      (vector-set! (tile-corners c) (tile-tile-index c b) n)
      (connect a n)
      (connect b n)
      (connect c n)
      n)))
  
(: cornerize-top (tile -> Void))
(define (cornerize-top t)
  (for ([n (edge-count t)])
    (when (empty-corner? (tile-corner t n))
      (add-corner t
                  (tile-tile t (- n 1))
                  (tile-tile t n)))))

(: top-grid (-> tile-set))
(define (top-grid)
  (let ([tiles (build-vector
                12
                (lambda: ([n : Integer])
                  (let ([coord (vector-ref icosahedron-coordinates n)])
                    (tile
                     empty-tile
                     coord
                     (vector)
                     (make-vector 5 empty-corner)
                     (coord->color coord)))))])
    (begin
      (for ([n 12])
        (set-tile-tiles!
         (vector-ref tiles n)
         (vector-map (lambda: ([k : Integer])
                       (vector-ref tiles k))
                     (vector-ref icosahedron-tiles n))))
      (for ([t tiles])
        (cornerize-top t))
      (list->set (vector->list tiles)))))

(: closest-tile (tile-set flvector3 -> tile))
(define (closest-tile tiles v)
  (: tile+dist (tile -> (List tile Flonum)))
  (define (tile+dist t)
    (list t (flvector3-distance-squared (tile-coordinates t)
                                        v)))
  (: closest (tile -> tile))
  (first (foldl (lambda: ([a : (List tile Flonum)]
                          [b : (List tile Flonum)])
                  (if (< (second a) (second b))
                      a b))
                (tile+dist (set-first tiles))
                (set-map tiles tile+dist))))

(: tile-section-coordinates (tile Integer -> flvector3))
(define (tile-section-coordinates t i)
  (flvector3-normal
   (flvector3+
    (tile-coordinates t)
    (corner-coordinates (tile-corner t i))
    (corner-coordinates (tile-corner t (- i 1))))))

(: corner-section-coordinates (corner Integer -> flvector3))
(define (corner-section-coordinates c i)
  (flvector3-normal
   (flvector3+
    (corner-coordinates c)
    (coordinates (parent-corner-at c i))
    (coordinates (parent-corner-at c (- i 1))))))

(: section-coordinates ((U tile corner) Integer -> flvector3))
(define (section-coordinates t i)
  (if (tile? t)
      (tile-section-coordinates t i)
      (corner-section-coordinates t i)))

(: cornerize (tile -> Void))
(define (cornerize t)
  (begin
    (for ([n (edge-count t)])
      (when (empty-corner? (tile-corner t n))
        (vector-set! (tile-corners t) n
                     (corner
                      (section-coordinates (tile-parent t) n)
                      (vector t (tile-tile t (- n 1)) (tile-tile t n))
                      (make-vector 3 empty-corner)))))
    (for ([n (edge-count t)])
      (let* ([c (tile-corner t n)]
             [i (corner-tile-index c t)])
        (vector-set! (corner-corners c) i
                     (tile-corner t (+ n 1)))
        (vector-set! (corner-corners c) (corner-index c (+ i 1))
                     (tile-corner t (- n 1)))))))

(: push (tile -> tile))
(define (push t)
  (let ([new-tile
         (tile
          t
          (tile-coordinates t)
          (make-vector (edge-count t) empty-tile)
          (make-vector (edge-count t) empty-corner)
          (tile-color t))])
    (begin
      (cornerize new-tile)
      new-tile)))

(: pop (tile -> tile-set))
(define (pop t)
  (let ([parent (guarantee (tile-parent t))])
    (if (or (empty-tile? parent) (and (tile? parent) (empty-tile? (tile-parent parent))))
        (top-grid)
        (set (if (tile? parent)
                 parent
                 (corner-tile parent 0))))))

(: add-left-corner (tile tile -> Void))
(define (add-left-corner old new)
  (let* ([i (tile-tile-index old new)]
         [k (tile-tile-index new old)]
         [c (tile-corner old (+ i 1))])
    (begin
      (vector-set! (tile-corners new) k c)
      (vector-set! (corner-tiles c) (corner-index c (+ (corner-tile-index c old) 1)) new))))

(: add-right-corner (tile tile -> Void))
(define (add-right-corner old new)
  (let* ([i (tile-tile-index old new)]
         [k (tile-tile-index new old)]
         [c (tile-corner old i)])
    (begin
      (vector-set! (tile-corners new) (tile-index new (+ k 1)) c)
      (vector-set! (corner-tiles c) (corner-index c (- (corner-tile-index c old) 1)) new))))

(: add-corners (tile tile -> Void))
(define (add-corners old new)
  (begin
    (add-right-corner old new)
    (add-left-corner old new)))

(: connect-tiles (tile tile tile -> Void))
(define (connect-tiles reference left right)
  (begin
    (vector-set! (tile-tiles left) (tile-index left (+ (tile-tile-index left reference) 1)) right)
    (vector-set! (tile-tiles right) (tile-index right (- (tile-tile-index right reference) 1)) left)))

(: add-left (tile tile -> Void))
(define (add-left t new)
  (let ([left (tile-tile t (+ (tile-tile-index t new) 1))])
    (unless (one-or-both true? (empty-tile? left) (no-empty? new))
      (begin
        (connect-tiles t left new)
        (add-left-corner left new)
        (add-left left new)))))

(: add-right (tile tile -> Void))
(define (add-right t new)
  (let ([right (tile-tile t (- (tile-tile-index t new) 1))])
    (unless (one-or-both true? (empty-tile? right) (no-empty? new))
      (begin
        (connect-tiles t new right)
        (add-right-corner right new)
        (add-right right new)))))

(: guarantee ((U tile corner) -> (U tile corner)))
(define (guarantee t)
  (begin
    (when (and (corner? t) (has-empty? t))
      (let ([n (first (filter (lambda (a)
                                (not (empty-tile? a)))
                              (vector->list (corner-tiles t))))])
        (for ([i (list (tile-corner-index n t)
                       (- (tile-corner-index n t) 1))])
          (when (empty-tile? (tile-tile n i))
            (add-tile n (tile-index n i))))))
    t))

(: add-tile (tile Integer -> tile))
(define (add-tile t i)
  (let* ([parent (guarantee (parent-at (tile-parent t) i))]
         [edge-count (if (corner? parent) 6 (edge-count parent))]
         [new-tile
          (tile
           parent
           (coordinates parent)
           (make-vector edge-count empty-tile)
           (make-vector edge-count empty-corner)
           (if (tile? parent)
               (tile-color parent)
               (coord->color (coordinates parent))))])
    (begin
      (let ([n (parent-parent-index parent (tile-parent t))])
        (vector-set! (tile-tiles t) i new-tile)
        (vector-set! (tile-tiles new-tile) n t))
      (add-corners t new-tile)
      (add-right t new-tile)
      (add-left t new-tile)
      (cornerize new-tile)
      new-tile)))

(: no-empty? ((U tile corner) -> Boolean))
(define (no-empty? t)
  (not (has-empty? t)))

(: has-empty? ((U tile corner) -> Boolean))
(define (has-empty? t)
  (not (not (vector-memq
             empty-tile
             (if (tile? t)
                 (tile-tiles t)
                 (corner-tiles t))))))

(: find-to-add (tile-set -> (List tile Index)))
(define (find-to-add tiles)
  (if (set-empty? tiles)
      (list empty-tile 0)
      (let ([t (set-first tiles)])
        (if (has-empty? t)
            (list t (tile-tile-index t empty-tile))
            (find-to-add (set-rest tiles))))))

(: add-one-tile (tile-set -> tile-set))
(define (add-one-tile tiles)
  (let ([ls (find-to-add tiles)])
    (if (empty-tile? (first ls))
        tiles
        (set-add tiles (add-tile (first ls) (second ls))))))

(: expand-to (flvector3 tile -> tile))
(define (expand-to v t)
  (let* ([coordinates (build-vector
                       (edge-count t)
                       (lambda: ([n : Integer])
                         (coordinates (parent-at (tile-parent t) n))))]
         [dist (lambda: ([n : (U False Integer)])
                 (flvector3-distance-squared
                  v (if (not n)
                        (tile-coordinates t)
                        (vector-ref coordinates n))))]
         [i (foldl (lambda: ([n : Integer]
                             [k : (U False Integer)])
                     (if (< (dist n) (dist k))
                         n k))
                   false
                   (range (edge-count t)))])
    (if (not i)
        t
        (expand-to v (add-tile t i)))))

(: prune (grid-constraint tile-set -> tile-set))
(define (prune f t)
  t)

(: expand (flvector3 Flonum tile-set -> tile-set))
(define (expand f radius tiles)
  tiles)
