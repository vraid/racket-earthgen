#lang typed/racket

(require racket/fixnum
         racket/vector
         "types.rkt"
         "index-vector.rkt"
         "vector3.rkt")

(provide tile
         tile?
         tile-id
         tile-tiles
         tile-corners
         tile-edges
         tile-tiles->vector
         tile-corners->vector
         tile-edges->vector
         tile-coordinates
         
         corner
         corner?
         corner-id
         corner-tiles
         corner-corners
         corner-edges
         corner-tiles->vector
         corner-corners->vector
         corner-edges->vector
         corner-coordinates
         
         edge
         edge?
         edge-id
         edge-tiles
         edge-corners
         edge-tiles->vector
         edge-corners->vector
         
         grid
         grid?
         grid-subdivision-level
         grid-tiles
         grid-corners
         grid-edges
         grid-tiles->vector
         grid-corners->vector
         grid-edges->vector
         
         tile-list
         corner-list
         edge-list
         tile-vector
         corner-vector
         edge-vector
         index-list
         tile-index-vector
         corner-index-vector
         edge-index-vector)

(define-type tile-list (Listof tile))
(define-type corner-list (Listof corner))
(define-type edge-list (Listof edge))
(define-type tile-vector (Vectorof tile))
(define-type corner-vector (Vectorof corner))
(define-type edge-vector (Vectorof edge))
(define-type tile-index-vector index-vector)
(define-type corner-index-vector index-vector3)
(define-type edge-index-vector index-vector2)

(struct: tile
  ([id : index]
   [coordinates : flvector3]
   [tiles->vector : tile-index-vector]
   [corners->vector : tile-index-vector]
   [edges->vector : tile-index-vector])
  #:transparent)

(struct: corner
  ([id : index]
   [coordinates : flvector3]
   [tiles->vector : corner-index-vector]
   [corners->vector : corner-index-vector]
   [edges->vector : corner-index-vector])
  #:transparent)

(struct: edge
  ([id : index]
   [tiles->vector : edge-index-vector]
   [corners->vector : edge-index-vector])
  #:transparent)

(struct: grid
  ([subdivision-level : index]
   [tiles->vector : tile-vector]
   [corners->vector : corner-vector]
   [edges->vector : edge-vector])
  #:transparent)

(: tile-tiles (tile -> index-list))
(define (tile-tiles a)
  (vector->list
   (tile-tiles->vector a)))

(: tile-corners (tile -> index-list))
(define (tile-corners a)
  (vector->list
   (tile-corners->vector a)))

(: tile-edges (tile -> index-list))
(define (tile-edges a)
  (vector->list
   (tile-edges->vector a)))

(: corner-tiles (corner -> index-list))
(define (corner-tiles a)
  (vector->list
   (corner-tiles->vector a)))

(: corner-corners (corner -> index-list))
(define (corner-corners a)
  (vector->list
   (corner-corners->vector a)))

(: corner-edges (corner -> index-list))
(define (corner-edges a)
  (vector->list
   (corner-edges->vector a)))

(: edge-tiles (edge -> index-list))
(define (edge-tiles a)
  (vector->list
   (edge-tiles->vector a)))

(: edge-corners (edge -> index-list))
(define (edge-corners a)
  (vector->list
   (edge-corners->vector a)))

(: grid-tiles (grid -> tile-list))
(define (grid-tiles a)
  (vector->list
   (grid-tiles->vector a)))

(: grid-corners (grid -> corner-list))
(define (grid-corners a)
  (vector->list
   (grid-corners->vector a)))

(: grid-edges (grid -> edge-list))
(define (grid-edges a)
  (vector->list
   (grid-edges->vector a)))
