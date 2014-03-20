#lang typed/racket

(provide make-int-array)

(require/typed "carray.rkt"
               [make-int-array (Integer -> (values (Integer -> Integer) (Integer Integer -> Void)))])
