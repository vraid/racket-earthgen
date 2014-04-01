#lang typed/racket

(provide make-int-array 
         (all-defined-out))

(require "types.rkt")
(require/typed "carray.rkt"
               [make-int-array (Integer -> (values (Integer -> Integer) (Integer Integer -> Void)))])

(: init-array (natural -> (case-> ((index Integer -> Void) (index -> Integer) -> Void)
                                  ((index Flonum -> Void) (index -> Flonum) -> Void)
                                  ((index Any -> Void) (index -> Any) -> Void))))
(define ((init-array length) set! get)
  (for ([n length])
    (set! n (get n))))
