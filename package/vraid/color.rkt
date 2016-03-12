#lang typed/racket

(provide (all-defined-out))

(require math/flonum)

(struct: byte-color
  ([red : Byte]
   [green : Byte]
   [blue : Byte]
   [alpha : Byte])
  #:transparent)

(struct: flcolor
  ([red : Float]
   [green : Float]
   [blue : Float]
   [alpha : Float])
  #:transparent)

(: flcolor3 (Float Float Float -> flcolor))
(define (flcolor3 red green blue)
  (flcolor red green blue 1.0))

(: flcolor->list (flcolor -> (Listof Float)))
(define (flcolor->list color)
  (list (flcolor-red color)
        (flcolor-green color)
        (flcolor-blue color)
        (flcolor-alpha color)))

(: flcolor->byte (Float -> Byte))
(define (flcolor->byte c)
  (let ([b (max 0
                (min 255
                     (inexact->exact
                      (round (fl* 255.0 c)))))])
    (if (byte? b)
        b
        0)))

(: flcolor->byte-color (flcolor -> byte-color))
(define (flcolor->byte-color c)
  (byte-color
   (flcolor->byte (flcolor-red c))
   (flcolor->byte (flcolor-green c))
   (flcolor->byte (flcolor-blue c))
   (flcolor->byte (flcolor-alpha c))))

(define flcolor-interpolate
  (lambda: ([col-one : flcolor]
            [col-two : flcolor]
            [d : Float])
    (let* ([1-d (fl- 1.0 d)]
           [f (lambda: ([f : (flcolor -> Float)])
                (fl+ (fl* 1-d (f col-one))
                     (fl* d (f col-two))))])
      (flcolor (f flcolor-red)
               (f flcolor-green)
               (f flcolor-blue)
               (f flcolor-alpha)))))
