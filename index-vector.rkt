#lang typed/racket

(require racket/vector)

(provide maybe-index
         index-vector
         index-vector2
         index-vector3
         index-vector5
         index-vector6)

(define-type maybe-index (U Integer False))

(define-type index-vector (Vectorof Fixnum))

(define-type index-vector2 (Vector Fixnum Fixnum))
(define-type index-vector3 (Vector Fixnum Fixnum Fixnum))
(define-type index-vector5 (Vector Fixnum Fixnum Fixnum Fixnum Fixnum))
(define-type index-vector6 (Vector Fixnum Fixnum Fixnum Fixnum Fixnum Fixnum))
