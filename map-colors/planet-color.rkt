#lang typed/racket

(require vraid/color
         math/flonum
         "color-base.rkt"
         "../planet/geometry/time.rkt"
         "../planet/climate/temperature.rkt"
         "../planet/climate/sunlight.rkt")

(provide (all-defined-out))

(define-type flcolor-list (Listof flcolor))

(: find-color (Float (Listof Float) flcolor-list -> flcolor))
(define (find-color tile-value intervals colors)
  (: rec-find ((Listof Float) flcolor-list (U Float Boolean) flcolor -> flcolor))
  (define (rec-find intervals colors last-interval last-color)
    (cond [(empty? intervals) color-undefined]
          [(< tile-value (first intervals))
           (if (boolean? last-interval)
               color-undefined
               (flcolor-interpolate last-color
                                    (first colors)
                                    (/ (- tile-value last-interval)
                                       (- (first intervals) last-interval))))]
          [else (rec-find (rest intervals)
                          (rest colors)
                          (first intervals)
                          (first colors))]))
  (if (< tile-value (first intervals))
      color-undefined
      (rec-find intervals
                colors
                #f
                color-undefined)))

(: filter-intervals ((Listof Any) -> (Listof Float)))
(define (filter-intervals ls)
  (filter flonum? ls))

(: filter-colors ((Listof Any) -> flcolor-list))
(define (filter-colors ls)
  (filter flcolor? ls))

(define topography-intervals/colors
  (list -10000.0 (flcolor3 0.0 0.0 0.0) 
        -3000.0 (flcolor3 0.0 0.02 0.08)
        -200.0 (flcolor3 0.09 0.27 0.49)
        0.0 (flcolor3 0.09 0.27 0.49)
        0.0 (flcolor3 0.66 0.59 0.45)
        1500.0 (flcolor3 0.41 0.35 0.22)
        3000.0 (flcolor3 0.09 0.13 0.04)))

(define topography-intervals
  (filter-intervals
   topography-intervals/colors))

(define topography-colors
  (filter-colors
   topography-intervals/colors))

(define color-topography
  (λ ([sea-level : Float]
      [tile-water? : (Integer -> Boolean)]
      [tile-elevation : (Integer -> Float)]
      [tile-water-depth : (Integer -> Float)])
    (λ ([n : Integer])
      (find-color (if (tile-water? n)
                      (- (tile-water-depth n))
                      (- (tile-elevation n) sea-level))
                  topography-intervals
                  topography-colors))))

(define water-surface
  (flcolor3 0.11372549019607843 0.3058823529411765 0.5686274509803921))
(define water-mid
  (flcolor3 0.06862745098039216 0.17450980392156862 0.37058823529411766))
(define water-deep
  (flcolor3 0.03137254901960784 0.0784313725490196 0.19215686274509802))
(define barren-land-low
  (flcolor3 0.6588235294117647 0.5686274509803921 0.4196078431372549))
(define barren-land-high
  (flcolor3 0.11764705882352941 0.10980392156862745 0.050980392156862744))

(define vegetation-topography-intervals/colors
  (list -10000.0 water-deep
        -3500.0 water-deep
        -1500.0 water-mid
        -200.0 water-surface
        0.0 water-surface
        0.0 barren-land-low
        2000.0 barren-land-high
        4000.0 barren-land-high))

(define vegetation-topography-intervals
  (filter-intervals
   vegetation-topography-intervals/colors))

(define vegetation-topography-colors
  (filter-colors
   vegetation-topography-intervals/colors))

(: scale/log (Float -> Float))
(define (scale/log n)
  (fllog2 (+ 1.0 n)))

(define dense-vegetation (flcolor3 0.09411764705882353 0.1568627450980392 0.03137254901960784))
(define medium-vegetation (flcolor3 0.13 0.22 0.045))
(define max-vegetation-scale (scale/log 7.0))
(define medium-vegetation-cutoff 2.0)

(define color-landscape
  (λ ([sea-level : Float]
      [tile-water? : (Integer -> Boolean)]
      [tile-elevation : (Integer -> Float)]
      [tile-water-depth : (Integer -> Float)]
      [tile-temperature : (Integer -> Float)]
      [tile-leaf-area-index : (Integer -> Float)])
    (λ ([n : Integer])
      (let* ([water? (tile-water? n)]
             [topography (find-color (if water?
                                         (- (tile-water-depth n))
                                         (- (tile-elevation n) sea-level))
                                     vegetation-topography-intervals
                                     vegetation-topography-colors)])
        (if water?
            topography
            (if (below-freezing-temperature? (tile-temperature n))
                snow-color
                (let ([leaf-area (scale/log (tile-leaf-area-index n))])
                  (if (< medium-vegetation-cutoff leaf-area)
                      (flcolor-interpolate/limit medium-vegetation
                                                 dense-vegetation
                                                 (/ (- leaf-area medium-vegetation-cutoff)
                                                    (- max-vegetation-scale medium-vegetation-cutoff)))
                      (flcolor-interpolate/limit topography
                                                 medium-vegetation
                                                 (/ leaf-area medium-vegetation-cutoff))))))))))

(define vegetation-min (flcolor3 1.0 1.0 1.0))
(define vegetation-max (flcolor3 0.0 0.3 0.0))

(define color-leaf-area-index
  (λ ([tile-water? : (Integer -> Boolean)]
      [tile-leaf-area-index : (Integer -> Float)])
    (λ ([n : Integer])
      (if (tile-water? n)
          color-neutral-water
          (flcolor-interpolate/limit vegetation-min
                                     vegetation-max
                                     (/ (scale/log (tile-leaf-area-index n))
                                        max-vegetation-scale))))))

(define temperature-intervals/colors
  (list (- freezing-temperature) (flcolor3 1.0 1.0 1.0)
        -70.0 (flcolor3 1.0 0.0 1.0)
        -50.0 (flcolor3 0.5 0.0 0.5)
        -30.0 (flcolor3 0.0 0.0 0.5)
        -10.0 (flcolor3 0.0 0.0 1.0)
        10.0 (flcolor3 1.0 1.0 0.0)
        30.0 (flcolor3 1.0 0.0 0.0)
        50.0 (flcolor3 0.5 0.0 0.0)
        70.0 (flcolor3 0.0 0.0 0.0)))

(: temperature-intervals (Listof Float))
(define temperature-intervals
  (map (lambda ([a : Float])
         (+ a freezing-temperature))
       (filter-intervals
        temperature-intervals/colors)))

(define temperature-colors
  (filter-colors
   temperature-intervals/colors))

(define color-temperature
  (λ ([tile-temperature : (Integer -> Float)])
    (λ ([n : Integer])
      (find-color (tile-temperature n)
                  temperature-intervals
                  temperature-colors))))

(define humidity-min (flcolor3 1.0 1.0 1.0))
(define humidity-max (flcolor3 0.0 1.0 0.0))
(define humidity-water (flcolor3 0.0 0.0 0.5))

(define color-humidity
  (λ ([tile-water? : (Integer -> Boolean)]
      [tile-relative-humidity : (Integer -> Float)])
    (λ ([n : Integer])
      (if (tile-water? n)
          humidity-water
          (flcolor-interpolate humidity-min
                               humidity-max
                               (tile-relative-humidity n))))))

(define precipitation-min (flcolor3 1.0 1.0 1.0))
(define precipitation-max (flcolor3 0.0 1.0 0.0))

(define millimeters-per-day (/ 0.001 seconds-per-day))
(define max-precipitation-scale (scale/log (* 3.0 millimeters-per-day)))

(define color-precipitation
  (λ ([tile-water? : (Integer -> Boolean)]
      [tile-precipitation : (Integer -> Float)])
    (λ ([n : Integer])
      (if (tile-water? n)
          humidity-water
          (flcolor-interpolate/limit precipitation-min
                                     precipitation-max
                                     (/ (scale/log (tile-precipitation n))
                                        max-precipitation-scale))))))

(define aridity-min color-neutral)
(define aridity-medium (flcolor3 1.0 1.0 0.0))
(define aridity-max (flcolor3 1.0 0.0 0.0))
(define aridity-water humidity-water)

(define color-aridity
  (λ ([tile-water? : (Integer -> Boolean)]
      [tile-aridity : (Integer -> Float)])
    (λ ([n : Integer])
      (if (tile-water? n)
          aridity-water
          (let ([aridity (tile-aridity n)])
            (if (> 1.0 aridity)
                (flcolor-interpolate aridity-min
                                     aridity-medium
                                     (/ aridity 1.0))
                (flcolor-interpolate/limit aridity-medium
                                           aridity-max
                                           (/ (- aridity 1.0)
                                              1.0))))))))

(define insolation-min color-neutral)
(define insolation-medium color-yellow)
(define insolation-max color-red)
(define insolation-water humidity-water)

(define color-insolation
  (λ ([tile-water? : (Integer -> Boolean)]
      [tile-insolation : (Integer -> Float)])
    (λ ([n : Integer])
      (if (tile-water? n)
          insolation-water
          (let ([insolation (/ (tile-insolation n)
                               solar-constant)])
            (if (> 0.5 insolation)
                (flcolor-interpolate insolation-min
                                     insolation-medium
                                     (/ insolation 0.5))
                (flcolor-interpolate insolation-medium
                                     insolation-max
                                     (/ (- insolation 0.5)
                                        0.5))))))))
