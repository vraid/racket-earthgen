#lang typed/racket

(require "planet.rkt"
         "color.rkt"
         "typed-logic.rkt")

(provide base-color
         color-temperature)

(define water-surface
  (color 0.0 0.4 0.8))
(define water-deep
  (color 0.0 0.0 0.3))
(define land-low
  (color 0.5 0.8 0.0))
(define land-high
  (color 0.2 0.2 0.1))

(define color-undefined
  (color 0.7 0.7 0.7))

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

(define temperature-zero
  (color 1.0 0.0 1.0))

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
       (filter (lambda: ([n : Any])
                 (flonum? n))
               temperature-intervals-colors)))

(define temperature-colors
  (filter (lambda: ([n : Any])
            (color? n))
          temperature-intervals-colors))

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
              [(and (flonum? last-interval)
                    (< tile-value (first intervals)))
               (color-interpolate last-color
                                  (first colors)
                                  (/ (- tile-value last-interval)
                                     (- (first intervals) last-interval)))]
              [(boolean? last-interval)
               color-undefined]
              [else
               (rec-find (rest intervals)
                         (rest colors)
                         (first intervals)
                         (first colors))])))
    (rec-find intervals
              colors
              #f
              color-undefined)))

(define color-temperature
  (lambda: ([tile : planet-tile])
    (find-color (planet-tile-temperature tile)
                temperature-intervals
                temperature-colors)))
