#lang typed/racket

(provide make-int-array 
         (all-defined-out))

(require/typed "carray.rkt"
               [make-int-array (Integer -> (values (Integer -> Integer) (Integer Integer -> Void)))])

(: init-array (Integer -> (case-> ((Integer Integer -> Void) (Integer -> Integer) -> Void)
                                  ((Integer Flonum -> Void) (Integer -> Flonum) -> Void)
                                  ((Integer Any -> Void) (Integer -> Any) -> Void))))
(define ((init-array length) set! get)
  (for ([n length])
    (set! n (get n))))
