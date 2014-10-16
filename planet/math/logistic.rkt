#lang typed/racket

(provide (all-defined-out))

(: logistic-growth-rate (Flonum Flonum Flonum -> Flonum))
(define (logistic-growth-rate growth-rate population carrying-capacity)
  (* growth-rate population (- 1 (/ population carrying-capacity))))

(: logistic-absolute (Flonum Flonum Flonum Flonum -> Flonum))
(define (logistic-absolute growth-rate population carrying-capacity time)
  (let ([ert (exp (* growth-rate time))])
    (/ (* carrying-capacity population ert)
       (+ carrying-capacity (* population
                               (- ert 1))))))
