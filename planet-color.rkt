#lang typed/racket

(require "planet.rkt"
         "color.rkt"
         "typed-logic.rkt")

(define color-undefined
  (color 0.7 0.7 0.7))

(provide base-color
         color-temperature
         color-topography)
(define find-color
  (lambda: ([tile-value : Flonum]
            [intervals : (Listof Flonum)]
            [colors : (Listof color)])
    (: rec-find ((Listof Flonum) (Listof color) (U Flonum Boolean) color -> color))
    (define rec-find
      (lambda: ([intervals : (Listof Flonum)]
                [colors : (Listof color)]
                [last-interval : (U Flonum Boolean)]
                [last-color : color])
        (cond [(empty? intervals)
               color-undefined]
              [(< tile-value (first intervals))
               (if (boolean? last-interval)
                   color-undefined
                   (color-interpolate last-color
                                      (first colors)
                                      (/ (- tile-value last-interval)
                                         (- (first intervals) last-interval))))]
              [else
               (rec-find (rest intervals)
                         (rest colors)
                         (first intervals)
                         (first colors))])))
    (rec-find intervals
              colors
              #f
              color-undefined)))

(define water-surface
  (color 0.0 0.4 0.8))
(define water-deep
  (color 0.0 0.0 0.3))
(define land-low
  (color 0.5 0.8 0.0))
(define land-high
  (color 0.2 0.2 0.1))

(define base-color-water
  (lambda: ([depth : Flonum])
    (color-interpolate
     water-surface
     water-deep
     (min 1.0 (/ depth 3000.0)))))

(define base-color-land
  (lambda: ([elevation : Flonum])
    (color-interpolate
     land-low
     land-high
     (min 1.0 (/ elevation 3000.0)))))

(define base-color
  (lambda: ([tile : planet-tile])
    (if (< 0 (planet-tile-water-depth tile))
        (base-color-water (planet-tile-water-depth tile))
        (base-color-land (planet-tile-elevation tile)))))

(define filter-intervals
  (lambda: ([ls : (Listof Any)])
    (filter (lambda: ([n : Any])
              (flonum? n))
            ls)))

(define filter-colors
  (lambda: ([ls : (Listof Any)])
    (filter (lambda: ([n : Any])
              (color? n))
            ls)))

(define topography-intervals-colors
  (list -10000.0 (color 0.0 0.0 0.0)
        -3000.0 (color 0.5 0.0 0.5)
        -2000.0 (color 0.0 0.0 0.5)
        -1000.0 (color 0.0 0.0 1.0)
        0.0 (color 0.0 1.0 1.0)
        0.0 (color 0.0 1.0 0.0)
        1000.0 (color 1.0 1.0 0.0)
        2000.0 (color 1.0 0.0 0.0)
        3000.0 (color 0.5 0.5 0.5)
        10000.0 (color 0.0 0.0 0.0)))

(define topography-intervals
  (filter-intervals
   topography-intervals-colors))

(define topography-colors
  (filter-colors
   topography-intervals-colors))

(define color-topography
  (lambda: ([tile : planet-tile])
    (find-color (planet-tile-elevation tile)
                topography-intervals
                topography-colors)))


(define freezing-temperature 273.15)

(define temperature-intervals-colors
  (list -90.0 (color 1.0 0.0 1.0)
        -70.0 (color 1.0 0.0 1.0)
        -50.0 (color 0.0 0.0 0.0)
        -30.0 (color 0.0 0.0 0.5)
        -10.0 (color 0.0 0.0 1.0)
        10.0 (color 0.0 1.0 0.0)
        30.0 (color 1.0 0.0 0.0)
        50.0 (color 0.5 0.0 0.0)
        70.0 (color 0.0 0.0 0.0)))

(define temperature-intervals
  (map (lambda: ([n : Flonum])
         (+ n freezing-temperature))
       (filter-intervals
        temperature-intervals-colors)))

(define temperature-colors
  (filter-colors
   temperature-intervals-colors))

(define color-temperature
  (lambda: ([tile : planet-tile])
    (find-color (planet-tile-temperature tile)
                temperature-intervals
                temperature-colors)))
