#lang racket

(provide pseudo-random-list?
         pseudo-random-list-numbers
         pseudo-random-list-next
         pseudo-random-list-rest
         make-pseudo-random-list)

(define (char-max byte-length)
  (vector-ref
   (vector-map (lambda (n) (+ n 1)) ; + 1 to prevent first characters of string->seed from being cancelled out by modulo
               (vector #x80
                       #x800
                       #x10000
                       #x200000
                       #x4000000
                       #x80000000))
   (- byte-length 1)))

(define seed-max (expt 2 31))

(define (string->seed s)
  (define (sum n chars)
    (if (empty? chars)
        n
        (sum (modulo
              (+ (char->integer (first chars))
                 (* n (char-max
                       (char-utf-8-length (first chars)))))
              seed-max)
             (rest chars))))
  (sum 0 (string->list s)))
               

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
  (random-seed (string->seed seed))
  (pseudo-random-list
   null
   (pseudo-random-generator->vector
    (current-pseudo-random-generator))))
