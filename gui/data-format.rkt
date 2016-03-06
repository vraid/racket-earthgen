#lang racket

(provide (all-defined-out))

(require vraid/flow
         math/flonum)

(define (string->float s)
  (and-let* ([n (string->number s)]
             [f (fl n)])
            f))

(define (float->exponential f)
  (if-not f "" (~r f
                   #:precision 3
                   #:notation 'exponential
                   #:format-exponent (lambda (e)
                                       (if (zero? e)
                                           ""
                                           (format " e~a" e))))))

(define (float->positional f)
  (if-not f "" (~r f #:precision 2 #:notation 'positional)))

(define (string->integer s)
  (and-let* ([n (string->number s)]
             [_ (integer? n)])
            (inexact->exact n)))

(define (integer->string i)
  (if-not i "" (number->string i)))

(struct convert
  (to-string from-string))

(define format-exponential
  (convert float->exponential
           string->float))

(define format-positional
  (convert float->positional
           string->float))

(define format-integer
  (convert integer->string
           string->integer))
