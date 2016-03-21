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

(: flcolor-interpolate (flcolor flcolor Float -> flcolor))
(define (flcolor-interpolate col-one col-two value)
  (let* ([1-value (fl- 1.0 value)]
         [f (lambda: ([f : (flcolor -> Float)])
              (fl+ (fl* 1-value (f col-one))
                   (fl* value (f col-two))))])
    (flcolor (f flcolor-red)
             (f flcolor-green)
             (f flcolor-blue)
             (f flcolor-alpha))))

(: flcolor-interpolate/limit (flcolor flcolor Float -> flcolor))
(define (flcolor-interpolate/limit one two value)
  (flcolor-interpolate one two (min 1.0 value)))
