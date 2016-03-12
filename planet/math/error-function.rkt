#lang typed/racket

(provide error-function)

(require math/flonum)

(define error-function-coefficients
  (flvector -1.26551223
            1.00002368
            0.37409196
            0.09678418
            -0.18628806
            0.27886807
            -1.13520398
            1.48851587
            -0.82215223
            0.17087277))

(: error-function (Float -> Float))
(define (error-function x)
  (let* ([t (/ 1.0 (+ 1.0 (* 0.5 (abs x))))]
         [tau (* t (exp (- (flvector-sum
                            (flvector* error-function-coefficients
                                       (build-flvector 10 (lambda (n)
                                                            (expt t (fl n))))))
                           (* x x))))])
    (if (< x 0)
        (- tau 1)
        (- 1 tau))))
