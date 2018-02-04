#lang typed/racket

(provide (all-defined-out))

(require "../grid-base.rkt"
         "heightmap-structs.rkt"
         "heightmap-create.rkt"
         math/flonum)

(: heightmap-map ((Float -> Float) heightmap-function -> heightmap-function))
(define (heightmap-map f h)
  (λ ([grids : grid-list])
    (let ([hmap (h grids)])
      (heightmap
       (flvector-map f (heightmap-tiles hmap))
       (flvector-map f (heightmap-corners hmap))))))

(: heightmap-map* ((Float Float Float * -> Float) heightmap-function heightmap-function heightmap-function * -> heightmap-function))
(define (heightmap-map* f h g . hs)
  (λ ([grids : grid-list])
    (let ([hmap (h grids)]
          [gmap (g grids)]
          [hmaps (map (λ ([h : heightmap-function]) (h grids))
                      hs)])
      (heightmap
       (apply flvector-map f
              (heightmap-tiles hmap)
              (heightmap-tiles gmap)
              (map heightmap-tiles hmaps))
       (apply flvector-map f
              (heightmap-corners hmap)
              (heightmap-corners gmap)
              (map heightmap-corners hmaps))))))

(: heightmap-raise (Float heightmap-function -> heightmap-function))
(define (heightmap-raise n h)
  (λ ([grids : grid-list])
    ((heightmap-map (curry fl+ (exact->inexact n))
                    h)
     grids)))

(: heightmap-lower (Float heightmap-function -> heightmap-function))
(define (heightmap-lower n h)
  (heightmap-raise (- n) h))

(: heightmap-add (heightmap-function heightmap-function -> heightmap-function))
(define (heightmap-add a b)
  (λ ([grids : grid-list])
    (let ([hmap-a (a grids)]
          [hmap-b (b grids)])
      (heightmap
       (flvector+ (heightmap-tiles hmap-a) (heightmap-tiles hmap-b))
       (flvector+ (heightmap-corners hmap-a) (heightmap-corners hmap-b))))))

(: heightmap-combine (heightmap-function heightmap-function * -> heightmap-function))
(define (heightmap-combine a . hs)
  (λ ([grids : grid-list])
    ((foldl (λ ([a : heightmap-function]
                [b : heightmap-function])
              (heightmap-add a b))
            a hs)
     grids)))
