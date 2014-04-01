(values
 3
 (let* ([continent (heightmap-map
                    (lambda (a)
                      (if (< 0 a)
                          a
                          (* 2.0 a)))
                    (heightmap-lower
                     250.0
                     (heightmap-create
                      (heightmap-parameters/kw
                       #:seed "dzig"
                       #:base-level 2
                       #:amplitude 800.0
                       #:persistence 0.65))))]
        [snakey (let ([width 0.4])
                  (heightmap-map
                   (lambda (a)
                     (/ (- width
                           (min width
                                (abs a)))
                        width))
                   (heightmap-create
                    (heightmap-parameters/kw
                     #:seed "snake"
                     #:base-level 3
                     #:amplitude 1.0
                     #:persistence 0.6))))]
        [snake-mountain (heightmap-map*
                         (lambda (a b . ns)
                           (* a
                              (abs b)))
                         snakey
                         (heightmap-create
                          (heightmap-parameters/kw
                           #:seed "mtn"
                           #:base-level 3
                           #:amplitude 2200.0
                           #:persistence 0.6)))]
                     
                 
        [mountain-base (heightmap-map*
                        (lambda (a b . ns)
                          (if (both
                               true?
                               (< -200.0 a)
                               (< 0.0 b))
                              b
                              0.0))
                        continent
                        (heightmap-raise
                         300.0
                         (heightmap-create
                          (heightmap-parameters/kw
                           #:seed "mtn2"
                           #:base-level 3
                           #:amplitude 800.0 
                           #:persistence 0.7))))]
        [mountain (heightmap-combine
                   continent
                   mountain-base)]
        [trench-base (heightmap-map*
                      (lambda (a b . ns)
                        (if (both
                             true?
                             (> -50.0 a)
                             (> 0.0 b))
                            b
                            0.0))
                      mountain
                      (heightmap-raise
                       1500.0
                       (heightmap-create
                        (heightmap-parameters/kw
                         #:seed "trench"
                         #:base-level 3
                         #:amplitude 2500.0
                         #:persistence 0.7))))]
        [final-terrain
         (heightmap-combine
          mountain
          trench-base)])
;   final-terrain

   (heightmap-combine
    continent
 snake-mountain)))
