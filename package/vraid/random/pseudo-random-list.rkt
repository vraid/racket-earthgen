#lang typed/racket

(provide pseudo-random-list
         pseudo-random-list?
         pseudo-random-list-numbers
         pseudo-random-list-next
         pseudo-random-list-rest
         make-pseudo-random-list)

(require math/flonum)

(define-type state-vector
  (Vector Positive-Integer
          Positive-Integer
          Positive-Integer
          Positive-Integer
          Positive-Integer
          Positive-Integer))

(: char-max (Integer -> Integer))
(define (char-max byte-length)
  (vector-ref
   (vector-map (lambda: ([n : Integer]) (+ n 1)) ; + 1 to prevent first characters of string->seed from being cancelled out by modulo
               (vector #x80
                       #x800
                       #x10000
                       #x200000
                       #x4000000
                       #x80000000))
   (- byte-length 1)))

(: seed-max Integer)
(define seed-max (- (expt 2 31) 1))

(: string->seed (String -> Positive-Integer))
(define (string->seed s)
  (: sum (Nonnegative-Integer (Listof Char) -> Positive-Integer))
  (define (sum n chars)
    (if (empty? chars)
        (+ n 1)
        (sum (abs (modulo
                   (+ (char->integer (first chars))
                      (* n (char-max
                            (char-utf-8-length (first chars)))))
                   seed-max))
             (rest chars))))
  (+ (sum 0 (string->list s)) 1))


(struct: pseudo-random-list
  ([numbers : FlVector]
   [state : state-vector])
  #:transparent)

(: pseudo-random-list-next (Integer pseudo-random-list -> pseudo-random-list))
(define (pseudo-random-list-next n r)
  (current-pseudo-random-generator
   (vector->pseudo-random-generator
    (pseudo-random-list-state r)))
  (let ([numbers (build-flvector n (lambda: ([n : Integer])
                                     (random)))]
        [state (pseudo-random-generator->vector
                (current-pseudo-random-generator))])
    (pseudo-random-list
     numbers
     state)))

(: pseudo-random-list-rest (pseudo-random-list -> pseudo-random-list))
(define (pseudo-random-list-rest r)
  (pseudo-random-list
   (flvector)
   (pseudo-random-list-state r)))

(: make-pseudo-random-list (String -> pseudo-random-list))
(define (make-pseudo-random-list seed)
  (random-seed (string->seed seed))
  (pseudo-random-list
   (flvector)
   (pseudo-random-generator->vector
    (current-pseudo-random-generator))))
