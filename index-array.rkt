#lang typed/racket

(provide make-index-array)

(require/typed "carray.rkt"
               [make-c-uint-array : (Natural -> (values (Integer -> Integer) (Integer -> Integer)))])
                                       
