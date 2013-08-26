#lang racket

(provide tile)
(provide tile-id)
(provide tile-tiles)
(provide tile-corners)
(provide tile-edges)
(provide tile-tiles->vector)
(provide tile-corners->vector)
(provide tile-edges->vector)
(provide tile-coordinates)

(provide corner)
(provide corner-id)
(provide corner-tiles)
(provide corner-corners)
(provide corner-edges)
(provide corner-tiles->vector)
(provide corner-corners->vector)
(provide corner-edges->vector)
(provide corner-coordinates)

(provide edge)
(provide edge-id)
(provide edge-tiles)
(provide edge-corners)
(provide edge-tiles->vector)
(provide edge-corners->vector)

(provide grid)
(provide grid-subdivision-level)
(provide grid-tiles)
(provide grid-corners)
(provide grid-edges)
(provide grid-tiles->vector)
(provide grid-corners->vector)
(provide grid-edges->vector)

(struct tile
  (id
   coordinates
   tiles->vector
   corners->vector
   edges->vector)
  #:transparent)

(struct corner
  (id
   coordinates
   tiles->vector
   corners->vector
   edges->vector)
  #:transparent)

(struct edge
  (id
   tiles->vector
   corners->vector)
  #:transparent)

(struct grid
  (subdivision-level
   tiles->vector
   corners->vector
   edges->vector)
  #:transparent)

(define (tile-tiles a)
  (vector->list
   (tile-tiles->vector a)))

(define (tile-corners a)
  (vector->list
   (tile-corners->vector a)))

(define (tile-edges a)
  (vector->list
   (tile-edges->vector a)))

(define (corner-tiles a)
  (vector->list
   (corner-tiles->vector a)))

(define (corner-corners a)
  (vector->list
   (corner-corners->vector a)))

(define (corner-edges a)
  (vector->list
   (corner-edges->vector a)))

(define (edge-tiles a)
  (vector->list
   (edge-tiles->vector a)))

(define (edge-corners a)
  (vector->list
   (edge-corners->vector a)))

(define (grid-tiles a)
  (vector->list
   (grid-tiles->vector a)))

(define (grid-corners a)
  (vector->list
   (grid-corners->vector a)))

(define (grid-edges a)
  (vector->list
   (grid-edges->vector a)))
