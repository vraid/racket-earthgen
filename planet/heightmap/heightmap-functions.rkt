#lang typed/racket

(require "../grid/grid-list.rkt"
         "heightmap-structs.rkt"
         "heightmap-create.rkt"
         math/flonum)

(provide heightmap-map
         heightmap-map*
         heightmap-lower
         heightmap-raise
         heightmap-add
         heightmap-combine)

(: heightmap-map ((Flonum -> Flonum) heightmap-function -> heightmap-function))
(define (heightmap-map f h)
  (lambda: ([grids : grid-list])
    (let ([hmap (h grids)])
      (heightmap
       (flvector-map f (heightmap-tiles hmap))
       (flvector-map f (heightmap-corners hmap))))))

(: heightmap-map* ((Flonum Flonum Flonum * -> Flonum) heightmap-function heightmap-function heightmap-function * -> heightmap-function))
(define (heightmap-map* f h g . hs)
  (lambda: ([grids : grid-list])
    (let ([hmap (h grids)]
          [gmap (g grids)]
          [hmaps (map (lambda: ([h : heightmap-function])
                        (h grids)) hs)])
      (heightmap
       (apply flvector-map f
              (heightmap-tiles hmap)
              (heightmap-tiles gmap)
              (map (lambda: ([h : heightmap])
                     (heightmap-tiles h)) hmaps))
       (apply flvector-map f
              (heightmap-corners hmap)
              (heightmap-corners gmap)
              (map (lambda: ([h : heightmap])
                     (heightmap-corners h)) hmaps))))))

(: heightmap-raise (Flonum heightmap-function -> heightmap-function))
(define (heightmap-raise n h)
  (lambda: ([grids : grid-list])
    ((heightmap-map
      (curry fl+ (exact->inexact n))
      h)
     grids)))

(: heightmap-lower (Flonum heightmap-function -> heightmap-function))
(define (heightmap-lower n h)
  (lambda: ([grids : grid-list])
    ((heightmap-raise (- n) h) grids)))

(: heightmap-add (heightmap-function heightmap-function -> heightmap-function))
(define (heightmap-add a b)
  (lambda: ([grids : grid-list])
    (let ([hmap-a (a grids)]
          [hmap-b (b grids)])
      (heightmap
       (flvector+ (heightmap-tiles hmap-a) (heightmap-tiles hmap-b))
       (flvector+ (heightmap-corners hmap-a) (heightmap-corners hmap-b))))))

(: heightmap-combine (heightmap-function heightmap-function * -> heightmap-function))
(define (heightmap-combine a . hs)
  (lambda: ([grids : grid-list])
    ((foldl (lambda: ([a : heightmap-function]
                      [b : heightmap-function])
              (heightmap-add a b))
            a hs)
     grids)))
