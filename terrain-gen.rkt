(let* ([continent (heightmap-map
                   (lambda (a)
                     (if (< 0 a)
                         a
                         (* 2.0 a)))
                   (heightmap-lower
                   300.0
                    (heightmap-create
                     (heightmap-parameters/kw
                      #:seed "nteqsd"
                      #:base-level 2
                      #:amplitude 800.0
                      #:persistence 0.65))))]
       [snakey (let ([width 0.3])
                 (heightmap-map
                  (lambda (a)
                    (sqrt
                    (/ (- width
                          (min width
                               (abs a)))
                       width)))
                  (heightmap-create
                   (heightmap-parameters/kw
                    #:seed "kserg"
                    #:base-level 2
                    #:amplitude 1.0
                    #:persistence 0.0))))]
       [mountain-mod
        (heightmap-raise
         600.0
         (heightmap-map*
          (lambda (a b . n)
            (max a b))
          (heightmap-create
           (heightmap-parameters/kw
            #:seed "mtkfg"
            #:base-level 2
            #:amplitude 2000.0
            #:persistence 0.7))
          (heightmap-create
           (heightmap-parameters/kw
            #:seed "kyjrd"
            #:base-level 2
            #:amplitude 2000.0
            #:persistence 0.7))))])
  (heightmap-combine
   continent
   (heightmap-map*
    (lambda (a b . ns)
      (* (max 0.0 a) b))
    mountain-mod
    snakey)))
