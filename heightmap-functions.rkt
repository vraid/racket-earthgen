#lang racket

(require "heightmap-structs.rkt"
         "heightmap-create.rkt"
         "math.rkt"
         math/flonum)

(provide heightmap-map
         heightmap-lower
         heightmap-raise
         heightmap-sum
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

(define (heightmap-combine a b)
  (lambda (grids)
    (let ([hmap-a (a grids)]
          [hmap-b (b grids)])
      (heightmap
       (flvector+ (heightmap-tiles hmap-a) (heightmap-tiles hmap-b))
       (flvector+ (heightmap-corners hmap-a) (heightmap-corners hmap-b))))))

(define (heightmap-sum hs)
  (lambda (grids)
    (if (empty? hs)
        #f
        (if (empty? (rest hs))
            ((first hs) grids)
            (foldl
             (lambda (a b) ((heightmap-combine a b) grids))
             hs)))))
