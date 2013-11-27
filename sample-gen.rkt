#lang typed/racket

(require "planet.rkt"
         "typed-logic.rkt"
         "heightmap-create.rkt"
         "heightmap-functions.rkt")

(provide sample-planet)

(define sample-planet
 (let* ([continent (heightmap-map
                    (lambda: ([a : Flonum])
                      (if (< 0 a)
                          a
                          (* 2 a)))
                    (heightmap-lower
                     70.0
                     (heightmap-create
                      (heightmap-parameters "earth99" 2 800.0 0.65))))]
        [mountain-base (heightmap-map*
                        (ann
                        (lambda (a b . bs)
                          (if (both
                               true?
                               (< -200.0 a)
                               (< 0.0 b))
                              b
                              0.0))
                        (Flonum Flonum Flonum * -> Flonum))
                        continent
                        (heightmap-raise
                         300.0
                         (heightmap-create
                          (heightmap-parameters "mtn2" 3 800.0 0.7))))]
        [mountain (heightmap-combine
                   continent
                   mountain-base)]
        [trench-base (heightmap-map*
                      (ann
                      (lambda (a b . ns)
                        (if (both
                             true?
                             (> -50.0 a)
                             (> 0.0 b))
                            b
                            0.0))
                      (Flonum Flonum Flonum * -> Flonum))
                      mountain
                      (heightmap-raise
                       1500.0
                       (heightmap-create
                        (heightmap-parameters "trench" 3 2500.0 0.7))))]                   
        [final-terrain
         (heightmap-combine
          mountain
          trench-base)])
   final-terrain))
