#lang racket

(require "heightmap-structs.rkt"
         "heightmap-create.rkt"
         "math.rkt"
         math/flonum)

(provide heightmap-map
         heightmap-lower
         heightmap-raise
         heightmap-add
         heightmap-combine)

(define (heightmap-map f . hs)
  (lambda (grids)
    (let ([hmaps (map (lambda (h) (h grids)) hs)])
      (heightmap
       (apply flvector-map f
              (map (lambda (h) (heightmap-tiles h)) hmaps))
       (apply flvector-map f
              (map (lambda (h) (heightmap-corners h)) hmaps))))))

(define (heightmap-raise n h)
  (lambda (grids)
    ((heightmap-map
      (lambda (a)
        (fl+ a (exact->inexact n)))
      h)
     grids)))

(define (heightmap-lower n t)
  (lambda (grids)
    ((heightmap-raise (- n) t) grids)))

(define (heightmap-add a b)
  (lambda (grids)
    (let ([hmap-a (a grids)]
          [hmap-b (b grids)])
      (heightmap
       (flvector+ (heightmap-tiles hmap-a) (heightmap-tiles hmap-b))
       (flvector+ (heightmap-corners hmap-a) (heightmap-corners hmap-b))))))

(define (heightmap-combine a . hs)
  (lambda (grids)
    (foldl (lambda (a b) ((heightmap-add a b) grids))
           a hs)))
