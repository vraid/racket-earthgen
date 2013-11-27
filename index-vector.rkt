#lang typed/racket

(require racket/vector
         "types.rkt")

(provide vector2
         vector3
         index-list
         index-vector
         index-vector2
         index-vector3
         maybe-index
         maybe-index-list
         maybe-index-vector
         maybe-index-vector2
         maybe-index-vector3)

(define-type (vector2 a) (Vector a a))
(define-type (vector3 a) (Vector a a a))

(define-type index-list (Listof index))
(define-type maybe-index (maybe index))
(define-type maybe-index-list (Listof maybe-index))

(define-type index-vector (Vectorof index))
(define-type index-vector2 (vector2 index))
(define-type index-vector3 (vector3 index))

(define-type maybe-index-vector (Vectorof maybe-index))
(define-type maybe-index-vector2 (vector2 maybe-index))
(define-type maybe-index-vector3 (vector3 maybe-index))
