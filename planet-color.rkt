#lang typed/racket

(provide (all-defined-out))

(require "types.rkt"
         "math.rkt"
         "color.rkt"
         "planet.rkt"
         "temperature.rkt"
         "typed-logic.rkt"
         racket/flonum)

(define color-undefined
  (flcolor3 0.7 0.7 0.7))

(define color-neutral
  (flcolor3 0.95 0.95 0.85))

(define-type flcolor-list (Listof flcolor))

(: find-color (Flonum (Listof Flonum) flcolor-list -> flcolor))
(define (find-color tile-value intervals colors)
  (: rec-find ((Listof Flonum) flcolor-list (U Flonum Boolean) flcolor -> flcolor))
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

(: filter-intervals ((Listof Any) -> (Listof Flonum)))
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

(: color-topography (planet index -> flcolor))
(define (color-topography p n)
  (find-color (tile-elevation p n)
              topography-intervals
              topography-colors))

(define vegetation-color (flcolor3 0.09411764705882353 0.1568627450980392 0.03137254901960784))
(define snow-color (flcolor3 0.9 0.9 0.9))

(define water-surface
  (flcolor3 0.06862745098039216 0.17450980392156862 0.37058823529411766))
(define water-deep
  (flcolor3 0.0 0.01568627450980392 0.06274509803921569))
(define water-mid
  (flcolor3 0.03137254901960784 0.0784313725490196 0.19215686274509802))
(define barren-land-low
  (flcolor3 0.6588235294117647 0.5686274509803921 0.4196078431372549))
(define barren-land-high
  (flcolor3 0.11764705882352941 0.10980392156862745 0.050980392156862744))

(define barren-land-low-level 0.0)
(define barren-land-high-level 3000.0)

(define vegetation-topography-intervals/colors
  (list -10000.0 water-deep
        -2000.0 water-deep
        -800.0 water-mid
        0.0 water-surface
        0.0 barren-land-low
        2000.0 barren-land-high
        3000.0 barren-land-high))

(define vegetation-topography-intervals
  (filter-intervals
   vegetation-topography-intervals/colors))

(define vegetation-topography-colors
  (filter-colors
   vegetation-topography-intervals/colors))

(: color-vegetation-topography (planet index -> flcolor))
(define (color-vegetation-topography p n)
  (find-color (tile-elevation p n)
              vegetation-topography-intervals
              vegetation-topography-colors))

(: color-vegetation (planet index -> flcolor))
(define (color-vegetation p n)
  (if (fl< 0.0 (tile-snow-cover p n))
      snow-color
      (flcolor-interpolate (color-vegetation-topography p n)
                           vegetation-color
                           (vegetation-cover (tile-vegetation p n)))))

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

(: temperature-intervals (Listof Flonum))
(define temperature-intervals
  (map (curry + freezing-temperature)
       (filter-intervals
        temperature-intervals/colors)))

(define temperature-colors
  (filter-colors
   temperature-intervals/colors))

(: color-temperature (planet index -> flcolor))
(define (color-temperature p n)
  (find-color (tile-temperature p n)
              temperature-intervals
              temperature-colors))

(define humidity-min (flcolor3 1.0 1.0 1.0))
(define humidity-max (flcolor3 0.0 1.0 0.0))
(define humidity-water (flcolor3 0.0 0.0 0.5))

(: color-humidity (planet index -> flcolor))
(define (color-humidity p n)
  (if (tile-water? p n)
      humidity-water
      (flcolor-interpolate humidity-min
                           humidity-max
                           (tile-relative-humidity p n))))

(define aridity-min color-neutral)
(define aridity-medium (flcolor3 1.0 1.0 0.0))
(define aridity-max (flcolor3 1.0 0.0 0.0))
(define aridity-water humidity-water)

(: color-aridity (planet index -> flcolor))
(define (color-aridity p n)
  (if (tile-water? p n)
      aridity-water
      (let ([aridity (aridity (tile-temperature p n)
                              (tile-humidity p n))])
        (if (> 1.0 aridity)
            (flcolor-interpolate aridity-min
                                 aridity-medium
                                 (/ aridity 1.0))
            (flcolor-interpolate aridity-medium
                                 aridity-max
                                 (/ (- (min 2.0 aridity)
                                       1.0)
                                    1.0))))))

(define albedo-min (flcolor3 0.0 0.0 0.1))
(define albedo-max (flcolor3 1.0 1.0 1.0))

(: color-albedo (planet index -> flcolor))
(define (color-albedo p n)
  (flcolor-interpolate albedo-min
                       albedo-max
                       (tile-albedo p n)))

(define area-min (flcolor3 0.0 0.0 0.0))
(define area-max (flcolor3 1.0 1.0 1.0))

(: color-area (Flonum -> (planet index -> flcolor)))
(define ((color-area largest-area) p n)
  (flcolor-interpolate area-min
                       area-max
                       (/ (tile-area p n)
                          largest-area)))
