(values
 6
 (let* ([continent (heightmap-map
                    (lambda (a)
                      (if (< 0 a)
                          a
                          (* 2.0 a)))
                    (heightmap-lower
                     70.0
                     (heightmap-create
                      (heightmap-parameters/kw
                       #:seed "earth04"
                       #:base-level 2
                       #:amplitude 800.0
                       #:persistence 0.65))))]
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
   final-terrain))
