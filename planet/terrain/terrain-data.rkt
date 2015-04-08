#lang racket

(provide (struct-out tile-terrain-data)
         (struct-out corner-terrain-data)
         make-tile-terrain-data
         make-corner-terrain-data)

(require vraid/array
         ffi/unsafe)

(struct-array tile-terrain-data
              ([elevation _float]
               [water-level _float]))

(struct-array corner-terrain-data
              ([elevation _float]
               [river-direction _int]))
