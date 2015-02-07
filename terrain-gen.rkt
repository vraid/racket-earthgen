(let* ([continent (heightmap-map
                   (lambda (a)
                     (if (< 0 a)
                         a
                         (* 2.0 a)))
                   (heightmap-create
                    (heightmap-parameters/kw
                     #:seed "poia2"
                     #:base-level 2
                     #:amplitude 800.0
                     #:persistence 0.65)))]
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
                    #:seed "friboz"
                    #:base-level 2
                    #:amplitude 1.0
                    #:persistence 0.0))))]
       [mountain-mod
        (heightmap-map*
         (lambda (a b . n)
           (max a b 0.0))
         (heightmap-raise 600.0
         (heightmap-create
          (heightmap-parameters/kw
           #:seed "jyzjs"
           #:base-level 3
           #:amplitude 3000.0
           #:persistence 0.7)))
         (heightmap-raise 400.0
         (heightmap-create
          (heightmap-parameters/kw
           #:seed "zhsa"
           #:base-level 2
           #:amplitude 2000.0
           #:persistence 0.7))))]
       [mountains
        (heightmap-map*
         (lambda (a b . n)
           (* a b))
         mountain-mod
         snakey)])
  (heightmap-lower
   200.0
   (heightmap-map*
    (lambda (a b . n)
      (let* ([max-scale (* 10 a)]
             [total (+ a (* b (sgn a)))])
        (if (< (abs max-scale) (abs total))
            max-scale
            total)))
    continent
    mountains))
  #;(heightmap-combine
   continent
   (heightmap-map*
    (lambda (a b . ns)
      (* (max 0.0 a) b))
    mountain-mod
    snakey)))
