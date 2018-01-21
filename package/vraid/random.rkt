#lang typed/racket

(require math/flonum)

(provide (struct-out seed)
         random-take
         make-seed
         string->seed
         seed->state)

(struct: seed
  ([value : Positive-Integer]))

(: char-max (Integer -> Integer))
(define (char-max byte-length)
  (vector-ref
   (vector-map (λ ([n : Integer]) (+ n 1)) ; + 1 to prevent first characters of string->seed from being cancelled out by modulo
               (vector #x80
                       #x800
                       #x10000
                       #x200000
                       #x4000000
                       #x80000000))
   (- byte-length 1)))

(: seed-max Positive-Integer)
(define seed-max (max 1 (- (expt 2 31) 1)))

(: make-seed (Integer -> seed))
(define (make-seed n)
  (seed (+ 1 (modulo n seed-max))))

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

(: seed->state (seed -> Pseudo-Random-Generator))
(define (seed->state seed)
  (random-seed (seed-value seed))
  (current-pseudo-random-generator))

(: random-take (Pseudo-Random-Generator Integer -> (Values FlVector Pseudo-Random-Generator)))
(define (random-take state n)
  (current-pseudo-random-generator state)
  (let ([nums (build-flvector n (λ ([n : Integer])
                                  (random)))]
        [state (current-pseudo-random-generator)])
    (values nums state)))
