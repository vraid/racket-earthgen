#lang typed/racket

(provide (all-defined-out))

(require vraid/flow
         math/flonum)

(: string->float (String -> (Option Float)))
(define (string->float s)
  (let ([n (string->number s)])
    (if-not (real? n) #f (fl n))))

(: float->exponential ((Option Float) -> String))
(define (float->exponential f)
  (if-not f "" (~r f
                   #:precision 3
                   #:notation 'exponential
                   #:format-exponent (lambda ([e : Integer])
                                       (if (zero? e)
                                           ""
                                           (format " e~a" e))))))

(: float->exponential ((Option Float) -> String))
(define (float->positional f)
  (if-not (real? f) "" (~r f #:precision 2 #:notation 'positional)))

(: string->integer (String -> (Option Integer)))
(define (string->integer s)
  (let ([n (string->number s)])
    (if-not (integer? n) #f (round (inexact->exact n)))))

(: integer->string ((Option Integer) -> String))
(define (integer->string i)
  (if-not i "" (number->string i)))

(: string->flvector (String -> (Option FlVector)))
(define (string->flvector s)
  #f)

(: flvector->string ((Option FlVector) -> String))
(define (flvector->string v)
  (if-not v "" (string-join (map float->positional (flvector->list v)))))

(struct (a) convert
  ([to-string : ((Option a) -> String)]
   [from-string : (String -> (Option a))]))

(define format-exponential
  (convert float->exponential
           string->float))

(define format-positional
  (convert float->positional
           string->float))

(define format-integer
  (convert integer->string
           string->integer))

(define format-flvector
  (convert flvector->string
           string->flvector))
