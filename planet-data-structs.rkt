#lang racket

(provide (struct-out tile-data)
         (struct-out corner-data)
         (struct-out edge-data)
         make-tile-data
         make-corner-data
         make-edge-data)

(require "carray.rkt"
         ffi/unsafe)

(struct-array tile-data
              ([elevation _float]
               [water-level _float]
               [temperature _float]
               [humidity _float]
               [precipitation _float]))

(struct-array corner-data
              ([elevation _float]
               [river-direction _uint8]))

(struct-array edge-data
              ([has-river? _bool]
               [river-flow _float]))
