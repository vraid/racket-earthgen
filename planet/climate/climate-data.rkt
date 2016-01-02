#lang racket

(provide (all-defined-out))

(require vraid/array
         ffi/unsafe)

(struct-array tile-climate-data
              ([sunlight _float]
               [temperature _float]
               [humidity _float]
               [precipitation _float]
               [snow-cover _float]))

(struct-array corner-climate-data
              ([river-flow _float]))

(struct-array edge-climate-data
              ([river-flow _float]
               [air-flow _float]))
