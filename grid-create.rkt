#lang typed/racket

(require racket/fixnum
         racket/flonum
         "types.rkt"
         "index-vector.rkt"
         "vector-util.rkt"
         "vector3.rkt"
         "grid-structs.rkt"
         "grid-functions.rkt")

(provide n-grid
         subdivided-grid)

(define-type partial-tile-list (Listof partial-tile))
(define-type partial-corner-list (Listof partial-corner))
(define-type partial-edge-list (Listof partial-edge))
(define-type partial-tile-vector (Vectorof partial-tile))
(define-type partial-corner-vector (Vectorof partial-corner))
(define-type partial-edge-vector (Vectorof partial-edge))
(define-type partial-tile-index-vector maybe-index-vector)
(define-type partial-corner-index-vector maybe-index-vector3)
(define-type partial-edge-index-vector maybe-index-vector2)

(: false-vector2 maybe-index-vector2)
(define false-vector2
  (vector #f #f))
(: false-vector3 maybe-index-vector3)
(define false-vector3
  (vector #f #f #f))
(: false-vector5 maybe-index-vector)
(define false-vector5
  (vector #f #f #f #f #f))
(: false-vector6 maybe-index-vector)
(define false-vector6
  (vector #f #f #f #f #f #f))

(: zero-vector2 index-vector2)
(define zero-vector2
  (vector 0 0))

(: zero-vector3 index-vector3)
(define zero-vector3
  (vector 0 0 0))

(: zero-vector6 index-vector)
(define zero-vector6
  (vector 0 0 0 0 0 0))

(struct: partial-tile
  ([id : index]
   [coordinates : flvector3]
   [tiles : tile-index-vector]
   [corners : partial-tile-index-vector]
   [edges : partial-tile-index-vector])
  #:transparent)

(: partial-tile-edge-count (partial-tile -> index))
(define (partial-tile-edge-count partial-tile)
  (if (> 12 (partial-tile-id partial-tile))
      5
      6))

(: partial-tile-tile (partial-tile Integer -> index))
(define (partial-tile-tile t n)
  (vector-ref (partial-tile-tiles t) (modulo n (partial-tile-edge-count t))))

(: partial-tile-corner (partial-tile Integer -> maybe-index))
(define (partial-tile-corner t n)
  (vector-ref (partial-tile-corners t) (modulo n (partial-tile-edge-count t))))

(: partial-tile-edge (partial-tile Integer -> maybe-index))
(define (partial-tile-edge t n)
  (vector-ref (partial-tile-edges t) (modulo n (partial-tile-edge-count t))))

(: partial-tile-tile-position (partial-tile index -> index))
(define (partial-tile-tile-position r t)
  (vector-index t (partial-tile-tiles r)))

(: partial-tile-corner-position (partial-tile index -> maybe-index))
(define (partial-tile-corner-position t c)
  (vector-member c (partial-tile-corners t)))

(: partial-tile-edge-position (partial-tile index -> maybe-index))
(define (partial-tile-edge-position t e)
  (vector-member e (partial-tile-edges t)))

(struct: partial-corner
  ([id : index]
   [first-tile : index]
   [position : index]
   [coordinates : (maybe flvector3)]
   [tiles : partial-corner-index-vector]
   [corners : partial-corner-index-vector]
   [edges : partial-corner-index-vector])
  #:transparent)

(: partial-corner-tile (partial-corner index -> maybe-index))
(define (partial-corner-tile c n)
  (vector-ref (partial-corner-tiles c)
              (fxmodulo n corner-edge-count)))

(struct: partial-edge
  ([id : index]
   [first-tile : index]
   [position : index]
   [tiles : partial-edge-index-vector]
   [corners : partial-edge-index-vector])
  #:transparent)

(struct: partial-grid
  ([subdivision-level : index]
   [tiles : (maybe partial-tile-vector)]
   [corners : (maybe partial-corner-vector)]
   [edges : (maybe partial-edge-vector)])
  #:transparent)

(: not-none? (maybe-index -> Boolean))
(define (not-none? n)
  (not (false? n)))

(define 0-grid-coordinates
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

(: 0-grid-tile-tiles (Vectorof index-vector))
(define 0-grid-tile-tiles
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

(: maybe-index->index (maybe-index -> index))
(define (maybe-index->index n)
  (if (not n)
      0
      n))

(: tile+corners+edges (partial-tile partial-tile-index-vector partial-tile-index-vector -> partial-tile))
(define (tile+corners+edges t c e)
  (partial-tile
   (partial-tile-id t)
   (partial-tile-coordinates t)
   (partial-tile-tiles t)
   c
   e))

(: partial-tiles (grid -> partial-tile-vector))
(define (partial-tiles grid)
  (define tiles (grid-tiles->vector grid))
  (define corners (grid-corners->vector grid))
  (define tile-count (grid-tile-count grid))
  (define corner-count (grid-corner-count grid))
  (vector-append
   (vector-map
    (lambda: ([t : tile])
      (let ([n-false-vector (if (= 5 (tile-edge-count t))
                                false-vector5
                                false-vector6)])
        (partial-tile
         (tile-id t)
         (tile-coordinates t)
         (vector-map (lambda: ([n : index]) 
                       (+ n tile-count))
                     (tile-corners->vector t))
         n-false-vector
         n-false-vector)))
    tiles)
   (vector-map
    (lambda: ([c : corner])
      (partial-tile
       (+ tile-count (corner-id c))
       (corner-coordinates c)
       (build-vector 6 (lambda: ([n : index])
                         (if (even? n)
                             (+ tile-count (corner-corner c (round (/ n 2))))
                             (corner-tile c (round (/ (- n 1) 2))))))
       false-vector6
       false-vector6))
    (grid-corners->vector grid))))

(: part-grid (index partial-tile-vector -> partial-grid))
(define (part-grid subdivision-level tile-structure)  
  (: f (index (List partial-tile-list partial-corner-list partial-edge-list) -> (List partial-tile-list partial-corner-list partial-edge-list)))
  (define (f tile-id ls)
    (define tiles (first ls))
    (define corners (second ls))
    (define edges (third ls))
    (if (= tile-id (subdivision-level-tile-count subdivision-level))
        (list tiles corners edges)
        (let ([tile (vector-ref tile-structure tile-id)])
          (: next-tile (index maybe-index-list maybe-index-list partial-corner-list partial-edge-list -> (List partial-tile-list partial-corner-list partial-edge-list)))
          (define (next-tile n tile-corners tile-edges corners edges)
            (let ([next-corner-id (if (empty? corners)
                                      0
                                      (+ 1 (partial-corner-id (first corners))))]
                  [next-edge-id (if (empty? edges)
                                    0
                                    (+ 1 (partial-edge-id (first edges))))]
                  [new-corner? (= tile-id (min tile-id (partial-tile-tile tile n) (partial-tile-tile tile (- n 1))))]
                  [new-edge? (< tile-id (partial-tile-tile tile n))])
              (if (= n (partial-tile-edge-count tile))
                  (list (cons 
                         (tile+corners+edges
                          tile
                          (list->vector (reverse tile-corners))
                          (list->vector (reverse tile-edges)))
                         tiles)
                        corners
                        edges)
                  (next-tile (+ 1 n)
                             (cons (if new-corner? next-corner-id false) tile-corners)
                             (cons (if new-edge? next-edge-id false) tile-edges)
                             (if new-corner?
                                 (cons (partial-corner 
                                        next-corner-id
                                        tile-id
                                        n
                                        #f
                                        false-vector3
                                        false-vector3
                                        false-vector3)
                                       corners)
                                 corners)
                             (if new-edge?
                                 (cons (partial-edge
                                        next-edge-id
                                        tile-id
                                        n
                                        false-vector2
                                        false-vector2) edges) edges)))))
          (f (+ 1 tile-id) (next-tile 0 null null corners edges)))))
  
  (let* ([ls (f 0 (list null null null))]
         [tiles (first ls)]
         [corners (second ls)]
         [edges (third ls)])
    (partial-grid subdivision-level
                  (list->vector (reverse tiles))
                  (list->vector (reverse corners))
                  (list->vector (reverse edges)))))

(: complete-tiles (partial-tile-vector -> tile-vector))
(define (complete-tiles tiles)
  (vector-map
   (lambda: ([t : partial-tile])
     (tile
      (partial-tile-id t)
      (partial-tile-coordinates t)
      (partial-tile-tiles t)
      (build-vector
       (partial-tile-edge-count t)
       (lambda: ([n : index])
         (maybe-index->index
          (if (not-none? (partial-tile-corner t n))
              (partial-tile-corner t n)
              (let* ([tn (vector-ref tiles (min (partial-tile-tile t n)
                                                (partial-tile-tile t (- n 1))))]
                     [offset (if (= n (partial-tile-tile-position t (partial-tile-id tn)))
                                 1
                                 0)])
                (partial-tile-corner tn (+ offset (partial-tile-tile-position tn (partial-tile-id t)))))))))
      (build-vector
       (partial-tile-edge-count t)
       (lambda: ([n : index])
         (maybe-index->index
          (if (not-none? (partial-tile-edge t n))
              (partial-tile-edge t n)
              (let ([tn (vector-ref tiles (partial-tile-tile t n))])
                (partial-tile-edge tn (partial-tile-tile-position tn (partial-tile-id t))))))))))
   tiles))

(: list->vector3 (All (a) ((Listof a) -> (vector3 a))))
(define (list->vector3 ls)
  (vector
   (first ls)
   (second ls)
   (third ls)))

(: complete-corners (tile-vector partial-corner-vector -> corner-vector))
(define (complete-corners tiles partial-corners)
  (vector-map
   (lambda: ([c : partial-corner])
     (let* ([id (partial-corner-id c)]
            [n (partial-corner-position c)]
            [t1 (vector-ref tiles (partial-corner-first-tile c))]
            [t2 (vector-ref tiles (tile-tile t1 (- n 1)))]
            [t3 (vector-ref tiles (tile-tile t1 n))])
       (define: tiles : tile-list (list t1 t2 t3))
       (corner
        id
        (flvector3-normal (foldl flvector3+ (flvector3-zero) (map tile-coordinates tiles)))
        (list->vector3
         (map tile-id
              tiles))
        (list->vector3
         (map (lambda: ([t : tile])
                (tile-corner t (+ 1 (tile-corner-position t id))))
              tiles))
        (list->vector3
         (map (lambda: ([t : tile])
                (tile-edge t (tile-corner-position t id)))
              tiles)))))
   partial-corners))

(: complete-edges (tile-vector partial-edge-vector -> edge-vector))
(define (complete-edges tiles edges)
  (vector-map
   (lambda: ([e : partial-edge])
     (let* ([id (partial-edge-id e)]
            [t (vector-ref tiles (partial-edge-first-tile e))]
            [pos (partial-edge-position e)])
       (edge
        id
        (vector (tile-id t) (tile-tile t pos))
        (vector (tile-corner t pos) (tile-corner t (+ 1 pos))))))
   edges))

(: complete-grid (partial-grid -> grid))
(define (complete-grid partial)
  (define tiles (complete-tiles (certainly (partial-grid-tiles partial)
                                           (vector))))
  (grid
   (partial-grid-subdivision-level partial)
   tiles
   (complete-corners tiles (certainly (partial-grid-corners partial)
                                      (vector)))
   (complete-edges tiles (certainly (partial-grid-edges partial)
                                    (vector)))))

(: 0-grid grid)
(define 0-grid
  (complete-grid
   (part-grid
    0
    (build-vector
     12
     (lambda: ([n : index])
       (partial-tile
        n
        (vector-ref 0-grid-coordinates n)
        (vector-ref 0-grid-tile-tiles n)
        false-vector5
        false-vector5))))))

(: subdivided-grid (grid -> grid))
(define (subdivided-grid g)
  (complete-grid
   (part-grid
    (+ 1 (grid-subdivision-level g))
    (partial-tiles g))))

(: n-grid (Nonnegative-Fixnum -> grid))
(define (n-grid n)
  (if (zero? n)
      0-grid
      (subdivided-grid (n-grid (- n 1)))))
