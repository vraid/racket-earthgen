#lang typed/racket

(provide color-topography
         color-vegetation
         color-temperature
         color-humidity
         color-albedo
         color-area)

(require "types.rkt"
         "color.rkt"
         "planet.rkt"
         "temperature.rkt"
         "typed-logic.rkt"
         racket/flonum)

(define color-undefined
  (flcolor 0.7 0.7 0.7))

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

(define water-surface
  (flcolor 0.0 0.4 0.8))
(define water-deep
  (flcolor 0.0 0.0 0.3))
(define land-low
  (flcolor 0.5 0.8 0.0))
(define land-high
  (flcolor 0.2 0.2 0.1))

(: filter-intervals ((Listof Any) -> (Listof Flonum)))
(define (filter-intervals ls)
  (filter flonum? ls))

(: filter-colors ((Listof Any) -> flcolor-list))
(define (filter-colors ls)
  (filter flcolor? ls))

(define topography-intervals/colors
  (list -10000.0 (flcolor 0.0 0.0 0.0) 
        -3000.0 (flcolor 0.0 0.02 0.08)
        -200.0 (flcolor 0.09 0.27 0.49)
        0.0 (flcolor 0.09 0.27 0.49)
        0.0 (flcolor 0.66 0.59 0.45)
        1500.0 (flcolor 0.41 0.35 0.22)
        3000.0 (flcolor 0.09 0.13 0.04)))

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

(define vegetation-color (flcolor 0.176 0.32 0.05))
(define snow-color (flcolor 0.9 0.9 0.9))

(: color-vegetation (planet index -> flcolor))
(define (color-vegetation p n)
  (if (fl< 0.0 (tile-snow-cover p n))
      snow-color
      (flcolor-interpolate (color-topography p n)
                           vegetation-color
                           (vegetation-cover (tile-vegetation p n)))))

(define temperature-intervals/colors
  (list (- freezing-temperature) (flcolor 1.0 1.0 1.0)
        -70.0 (flcolor 1.0 0.0 1.0)
        -50.0 (flcolor 0.5 0.0 0.5)
        -30.0 (flcolor 0.0 0.0 0.5)
        -10.0 (flcolor 0.0 0.0 1.0)
        10.0 (flcolor 1.0 1.0 0.0)
        30.0 (flcolor 1.0 0.0 0.0)
        50.0 (flcolor 0.5 0.0 0.0)
        70.0 (flcolor 0.0 0.0 0.0)))

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

(define humidity-min (flcolor 1.0 1.0 1.0))
(define humidity-max (flcolor 0.0 1.0 0.0))
(define humidity-water (flcolor 0.0 0.0 0.5))

(: color-humidity (planet index -> flcolor))
(define (color-humidity p n)
  (if (tile-water? p n)
      humidity-water
      (flcolor-interpolate humidity-min
                           humidity-max
                           (tile-relative-humidity p n))))

(define albedo-min (flcolor 0.0 0.0 0.1))
(define albedo-max (flcolor 1.0 1.0 1.0))

(: color-albedo (planet index -> flcolor))
(define (color-albedo p n)
  (flcolor-interpolate albedo-min
                       albedo-max
                       (tile-albedo p n)))

(define area-min (flcolor 0.0 0.0 0.0))
(define area-max (flcolor 1.0 1.0 1.0))

(: color-area (planet index -> flcolor))
(define (color-area p n)
  (flcolor-interpolate area-min
                       area-max
                       (min 1.0
                            (/ (tile-area p n)
                               (tile-area p (- (tile-count p) 1))))))
