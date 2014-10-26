#lang racket

(provide (struct-out tile-data)
         (struct-out corner-data)
         (struct-out edge-data)
         make-tile-data
         make-corner-data
         make-edge-data)

(require vraid/array
         ffi/unsafe)

(struct-array tile-data
              ([elevation _float]
               [water-level _float]
               [sunlight _float]
               [temperature _float]
               [humidity _float]
               [precipitation _float]
               [snow-cover _float]
               [vegetation _float]
               [vertical-air-flow _float]
               [tropopause-altitude _float]))

(struct-array corner-data
              ([elevation _float]
               [river-direction _int]))

(struct-array edge-data
              ([river-flow _float]
               [air-flow _float]))
