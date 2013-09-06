#lang racket

(require "terrain-structs.rkt")
(provide terrain
         terrain-tile-elevations
         terrain-corner-elevations
         terrain-tile-water-levels
         terrain-tile-elevation
         terrain-corner-elevation
         terrain-tile-water-level)

(require "terrain-create.rkt")
(provide terrain-create
         terrain-parameters
         terrain
         terrain-tile-elevation
         terrain-corner-elevation)

(require "terrain-functions.rkt")
(provide terrain-elevation-map
         terrain-elevation-lower
         terrain-elevation-raise)  