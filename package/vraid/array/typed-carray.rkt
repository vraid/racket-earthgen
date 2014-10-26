#lang typed/racket

(provide make-int-array 
         (all-defined-out))

(require "../types.rkt")
(require/typed "carray.rkt"
               [make-int-array (Integer -> (values (Integer -> Integer) (Integer Integer -> Void)))])

(: init-array (integer -> (case-> ((integer Integer -> Void) (integer -> Integer) -> Void)
                                  ((integer Flonum -> Void) (integer -> Flonum) -> Void)
                                  ((integer Any -> Void) (integer -> Any) -> Void))))
(define ((init-array length) set! get)
  (for ([n length])
    (set! n (get n))))
