#lang typed/racket

(provide (all-defined-out))

(require math/flonum)

(struct: byte-color
  ([red : Byte]
   [green : Byte]
   [blue : Byte])
  #:transparent)

(struct: flcolor
  ([red : Flonum]
   [green : Flonum]
   [blue : Flonum])
  #:transparent)

(: flcolor->byte (Flonum -> Byte))
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
   (flcolor->byte (flcolor-blue c))))

(define flcolor-interpolate
  (lambda: ([col-one : flcolor]
            [col-two : flcolor]
            [d : Flonum])
    (let ([1-d (fl- 1.0 d)])
      (flcolor (fl+ (fl* 1-d (flcolor-red col-one))
                    (fl* d (flcolor-red col-two)))
               (fl+ (fl* 1-d (flcolor-green col-one))
                    (fl* d (flcolor-green col-two)))
               (fl+ (fl* 1-d (flcolor-blue col-one))
                    (fl* d (flcolor-blue col-two)))))))
