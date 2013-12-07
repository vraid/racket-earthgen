#lang typed/racket

(require "grid-structs.rkt")
(provide tile
         tile?
         tile-id
         tile-tiles
         tile-corners
         tile-edges
         tile-tiles->vector
         tile-corners->vector
         tile-edges->vector
         tile-coordinates
         
         corner
         corner?
         corner-id
         corner-tiles
         corner-corners
         corner-edges
         corner-tiles->vector
         corner-corners->vector
         corner-edges->vector
         corner-coordinates
         
         edge
         edge?
         edge-id
         edge-tiles
         edge-corners
         edge-tiles->vector
         edge-corners->vector
         
         grid
         grid?
         grid-subdivision-level
         grid-tiles
         grid-corners
         grid-edges
         grid-tiles->vector
         grid-corners->vector
         grid-edges->vector
         
         tile-vector
         corner-vector
         edge-vector
         index-list
         tile-index-vector
         corner-index-vector
         edge-index-vector)

(require "grid-functions.rkt")
(provide tile-edge-count
         tile-tile
         tile-corner
         tile-edge
         tile-tile-position
         tile-corner-position
         tile-edge-position
         
         corner-edge-count
         corner-tile
         corner-corner
         corner-edge
         corner-tile-position
         corner-corner-position
         corner-edge-position
         
         edge-tile
         edge-corner
         edge-tile-sign
         edge-corner-sign
         
         subdivision-level-tile-count
         subdivision-level-corner-count
         subdivision-level-edge-count
         grid-tile-count
         grid-corner-count
         grid-edge-count
         
         grid-tile
         grid-corner
         grid-edge)

(require "grid-create.rkt")
(provide n-grid
         subdivided-grid)

(require "grid-list.rkt")
(provide grid-list
         n-grid-list)