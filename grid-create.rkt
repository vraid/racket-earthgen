#lang racket

(require racket/fixnum
         "util-vector.rkt"
         "vector3.rkt"
         "grid-structs.rkt"
         "grid-functions.rkt")

(provide n-grid
         subdivided-grid)

(define none -1)
(define (not-none? n)
  (not (= none n)))

(define 0-grid-coordinates
  (let* ([x 0.525731112119133606]
         [z 0.850650808352039932]
         [-x (- x)]
         [-z (- z)])
    (vector
     (vector3 x 0.0 -z)
     (vector3 -x 0.0 -z)
     (vector3 x 0.0 z)
     (vector3 -x 0.0 z)
     (vector3 0.0 -z -x)
     (vector3 0.0 -z x)
     (vector3 0.0 z -x)
     (vector3 0.0 z x)
     (vector3 -z -x 0.0)
     (vector3 z -x 0.0)
     (vector3 -z x 0.0)
     (vector3 z x 0.0))))

(define 0-grid-tile-tiles
  (vector
   (fxvector 1 6 11 9 4)
   (fxvector 0 4 8 10 6)
   (fxvector 3 5 9 11 7)
   (fxvector 2 7 10 8 5)
   (fxvector 0 9 5 8 1)
   (fxvector 2 3 8 4 9)
   (fxvector 0 1 10 7 11)
   (fxvector 2 11 6 10 3)
   (fxvector 1 4 5 3 10)
   (fxvector 0 11 2 5 4)
   (fxvector 1 8 3 7 6)
   (fxvector 0 6 7 2 9)))

(struct partial-corner
  (id
   tile
   position))

(struct partial-edge
  (id
   tile
   position))

(define (tile+corners+edges t c e)
  (tile
   (tile-id t)
   (tile-coordinates t)
   (tile-tiles->vector t)
   c
   e))

(define (partial-tiles grid)
  (define tiles (grid-tiles->vector grid))
  (define corners (grid-corners->vector grid))
  (define tile-count (grid-tile-count grid))
  (define corner-count (grid-corner-count grid))
  (vector-append
   (vector-map
    (lambda (t)
      (tile
       (tile-id t)
       (tile-coordinates t)
       (fxvector-map (lambda (n) (+ n tile-count))
                     (tile-corners->vector t))
       #f
       #f))
    tiles)
   (vector-map
    (lambda (c)
      (tile
       (+ tile-count (corner-id c))
       (corner-coordinates c)
       (build-fxvector 6 (lambda (n) (if (even? n) (+ tile-count (corner-corner c (/ n 2))) (corner-tile c (/ (- n 1) 2)))))
       #f
       #f))
    (grid-corners->vector grid))))

(define (partial-grid subdivision-level tile-structure)
  (define (f tile-id ls)
    (define tiles (first ls))
    (define corners (second ls))
    (define edges (third ls))
    (if (= tile-id (subdivision-level-tile-count subdivision-level))
        (grid subdivision-level (list->vector (reverse tiles)) (reverse corners) (reverse edges))
        (let ([tile (vector-ref tile-structure tile-id)])
          (define (next-tile n tile-corners tile-edges corners edges)
            (let ([next-corner-id (if (empty? corners) 0 (+ 1 (partial-corner-id (first corners))))]
                  [next-edge-id (if (empty? edges) 0 (+ 1 (partial-edge-id (first edges))))]
                  [new-corner? (= tile-id (min tile-id (tile-tile tile n) (tile-tile tile (- n 1))))]
                  [new-edge? (< tile-id (tile-tile tile n))])
              (if (= n (tile-edge-count tile))
                  (list (cons 
                         (tile+corners+edges
                          tile
                          (list->fxvector n (reverse tile-corners))
                          (list->fxvector n (reverse tile-edges)))
                         tiles)
                        corners
                        edges)
                  (next-tile (+ 1 n)
                             (cons (if new-corner? next-corner-id none) tile-corners)
                             (cons (if new-edge? next-edge-id none) tile-edges)
                             (if new-corner? (cons (partial-corner next-corner-id tile-id n) corners) corners)
                             (if new-edge? (cons (partial-edge next-edge-id tile-id n) edges) edges)))))
          (f (+ 1 tile-id) (next-tile 0 null null corners edges)))))
  (f 0 (list null null null)))

(define (complete-grid partial-grid)
  (define tiles (complete-tiles (grid-tiles->vector partial-grid)))
  (grid
   (grid-subdivision-level partial-grid)
   tiles
   (complete-corners tiles (grid-corners->vector partial-grid))
   (complete-edges tiles (grid-edges->vector partial-grid))))

(define (complete-tiles tiles)
  (vector-map
   (lambda (t)
     (tile+corners+edges
      t
      (build-fxvector (tile-edge-count t) (lambda (n)
                      (if (not-none? (tile-corner t n))
                          (tile-corner t n)
                          (let* ([tn (vector-ref tiles (min (tile-tile t n)
                                                            (tile-tile t (- n 1))))]
                                 [offset (if (= n (tile-tile-position t (tile-id tn))) 1 0)])
                            (tile-corner tn (+ offset (tile-tile-position tn (tile-id t))))))))
      (build-fxvector (tile-edge-count t) (lambda (n)
                      (if (not-none? (tile-edge t n))
                          (tile-edge t n)
                          (let ([tn (vector-ref tiles (tile-tile t n))])
                            (tile-edge tn (tile-tile-position tn (tile-id t)))))))))
   tiles))

(define (complete-corners tiles partial-corners)
  (list->vector
   (map
    (lambda (c)
      (let* ([id (partial-corner-id c)]
             [n (partial-corner-position c)]
             [t1 (vector-ref tiles (partial-corner-tile c))]
             [t2 (vector-ref tiles (tile-tile t1 (- n 1)))]
             [t3 (vector-ref tiles (tile-tile t1 n))]
             [tiles (list t1 t2 t3)])
        (corner
         id
         (vector3-normal (foldl vector3+ vector3-zero (map tile-coordinates tiles)))
         (list->fxvector
          3
          (map tile-id
               tiles))
         (list->fxvector
          3
          (map (lambda (t) (tile-corner t (+ 1 (tile-corner-position t id))))
               tiles))
         (list->fxvector
          3
          (map (lambda (t) (tile-edge t (tile-corner-position t id)))
               tiles)))))
    partial-corners)))

(define (complete-edges tiles edges)
  (list->vector
   (map
    (lambda (e)
      (let* ([id (partial-edge-id e)]
             [t (vector-ref tiles (partial-edge-tile e))]
             [pos (partial-edge-position e)])
        (edge
         id
         (fxvector (tile-id t) (tile-tile t pos))
         (fxvector (tile-corner t pos) (tile-corner t (+ 1 pos))))))
    edges)))

(define 0-grid
  (complete-grid
   (partial-grid
    0
    (build-vector
     12
     (lambda (n)
       (tile
        n
        (vector-ref 0-grid-coordinates n)
        (vector-ref 0-grid-tile-tiles n)
        #f
        #f))))))

(define (subdivided-grid g)
  (let ([tiles (partial-tiles g)])
    (complete-grid
     (partial-grid
      (+ 1 (grid-subdivision-level g))
      tiles))))

(define (n-grid n)
  (if (zero? n)
      0-grid
      (subdivided-grid (n-grid (- n 1)))))
