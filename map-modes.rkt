#lang typed/racket

(require vraid/color
         "map-colors/planet-color.rkt"
         "planet/planet.rkt")

(provide (all-defined-out))

(struct map-mode
  ([name : Symbol]
   [function : (Integer -> flcolor)])
  #:transparent)

(: terrain-map-modes (planet-terrain -> (Listof map-mode)))
(define (terrain-map-modes planet)
  (list (map-mode 'topography (color-topography
                               (planet-sea-level planet)
                               (curry tile-land? planet)
                               (curry tile-elevation planet)
                               (curry tile-water-depth planet)))))

(: landscape-map-mode (planet-climate -> map-mode))
(define (landscape-map-mode planet)
  (map-mode 'landscape (color-landscape
                        (planet-sea-level planet)
                        (curry tile-water? planet)
                        (curry tile-elevation planet)
                        (curry tile-water-depth planet)
                        (curry tile-temperature planet)
                        (curry tile-leaf-area-index planet))))

(: climate-map-modes (planet-climate -> (Listof map-mode)))
(define (climate-map-modes planet)
  (let ([tile-water? (curry tile-water? planet)])
    (list
     (landscape-map-mode planet)
     (map-mode 'vegetation (color-leaf-area-index
                            tile-water?
                            (curry tile-leaf-area-index planet)))
     (map-mode 'temperature (color-temperature
                             (curry tile-temperature planet)))
     (map-mode 'insolation (color-insolation
                            tile-water?
                            (curry tile-insolation planet)))
     (map-mode 'aridity (color-aridity
                         tile-water?
                         (curry tile-aridity planet)))
     (map-mode 'humidity (color-humidity
                          tile-water?
                          (curry tile-relative-humidity planet)))
     (map-mode 'precipitation (color-precipitation
                               tile-water?
                               (curry tile-precipitation planet))))))
