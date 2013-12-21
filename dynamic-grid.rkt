#lang typed/racket

(provide (all-defined-out)
         (all-from-out "dynamic-grid-structs.rkt"
                       "dynamic-grid-access.rkt"))

(require "dynamic-grid-structs.rkt"
         "dynamic-grid-access.rkt"
         "vector3.rkt"
         math/flonum)

(define empty-corner
  (corner (flvector3-zero) (vector) (vector)))

(define empty-tile
  (tile empty-corner (flvector3-zero) (vector) (vector)))

(define (empty-tile? t)
  (eq? t empty-tile))

(define (empty-corner? c)
  (eq? c empty-corner))

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
        (connect-corners t c (tile-corner t (tile-index t (+ 1 (tile-corner-index t c))))))
      
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
                  (tile-tile t (tile-index t (- n 1)))
                  (tile-tile t n)))))

(: top-grid (-> tile-set))
(define (top-grid)
  (let ([tiles (build-vector
                12
                (lambda: ([n : Integer])
                  (tile
                   empty-tile
                   (vector-ref icosahedron-coordinates n)
                   (vector)
                   (make-vector 5 empty-corner))))])
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

(: tile-section-coordinates (tile Index -> flvector3))
(define (tile-section-coordinates t i)
  (flvector3-normal
   (flvector3+
    (tile-coordinates t)
    (corner-coordinates (tile-corner t i))
    (corner-coordinates (tile-corner t (tile-index t (- i 1)))))))

(: corner-section-coordinates (corner Index -> flvector3))
(define (corner-section-coordinates c i)
  (flvector3-zero))

(: section-coordinates ((U tile corner) Index -> flvector3))
(define (section-coordinates t i)
  (if (tile? t)
      (tile-section-coordinates t i)
      (corner-section-coordinates t i)))

(: cornerize (tile -> Void))
(define (cornerize t)
  (begin
    (for ([n (edge-count t)])
      (if (not (tile-corner t n))
          (vector-set! (tile-corners t) n
                       (corner
                        (section-coordinates (tile-parent t) n)
                        (vector)
                        (vector)))
          (void)))))

(: push (tile -> tile))
(define (push t)
  (let ([new-tile
         (tile
          t
          (tile-coordinates t)
          (make-vector (edge-count t) empty-tile)
          (make-vector (edge-count t) empty-corner))])
    (begin
      (for ([n (edge-count t)])
        (vector-set! (tile-corners new-tile) n
                     (corner
                      (flvector3-normal
                       (flvector3+
                        (tile-coordinates t)
                        (corner-coordinates (tile-corner t (tile-index t n)))
                        (corner-coordinates (tile-corner t (tile-index t (- n 1))))))
                      (vector new-tile empty-tile empty-tile)
                      (make-vector 3 empty-corner))))
      (for ([n (edge-count t)])
        (let ([c (tile-corner new-tile n)])
          (set-corner-corners! c (vector
                                  (tile-corner new-tile (tile-index t (+ n 1)))
                                  (tile-corner new-tile (tile-index t (- n 1)))
                                  empty-corner))))
      new-tile)))

(: add-one-tile (tile-set -> tile-set))
(define (add-one-tile tiles)
  tiles)

(: prune ((tile -> Boolean) tile-set -> tile-set))
(define (prune f t)
  t)

(: expand ((tile -> Boolean) tile-set -> tile-set))
(define (expand f t)
  t)
