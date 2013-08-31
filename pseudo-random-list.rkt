#lang racket

(provide pseudo-random-list?)
(provide pseudo-random-list-numbers)
(provide pseudo-random-list-next)
(provide pseudo-random-list-rest)
(provide make-pseudo-random-list)

(struct pseudo-random-list
  (numbers
   state)
  #:transparent)

(define (pseudo-random-list-next n r)
  (current-pseudo-random-generator
   (vector->pseudo-random-generator
    (pseudo-random-list-state r)))
  (let ([numbers (build-list n (lambda (n) (random)))]
        [state (pseudo-random-generator->vector (current-pseudo-random-generator))])
    (pseudo-random-list
     numbers
     state)))

(define (pseudo-random-list-rest r)
  (pseudo-random-list
   null
   (pseudo-random-list-state r)))

(define (make-pseudo-random-list seed)
  (random-seed seed)
  (pseudo-random-list
   null
   (pseudo-random-generator->vector
    (current-pseudo-random-generator))))
