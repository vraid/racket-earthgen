#lang typed/racket

(require math/flonum)

(provide color
         color?
         color-red
         color-green
         color-blue
         color-interpolate)

(struct: color
  ([red : Flonum]
   [green : Flonum]
   [blue : Flonum]))

(define color-interpolate
  (lambda: ([col-one : color]
            [col-two : color]
            [d : Flonum])
    (let ([1-d (fl- 1.0 d)])
      (color (fl+ (fl* 1-d (color-red col-one))
                  (fl* d (color-red col-two)))
             (fl+ (fl* 1-d (color-green col-one))
                  (fl* d (color-green col-two)))
             (fl+ (fl* 1-d (color-blue col-one))
                  (fl* d (color-blue col-two)))))))
