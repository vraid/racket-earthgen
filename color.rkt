#lang typed/racket

(require math/flonum)

(provide flcolor
         flcolor?
         flcolor-red
         flcolor-green
         flcolor-blue
         flcolor-interpolate)

(struct: flcolor
  ([red : Flonum]
   [green : Flonum]
   [blue : Flonum]))

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
