#lang racket

(provide (struct-out tile-climate-data)
         (struct-out edge-climate-data)
         make-tile-climate-data
         make-edge-climate-data)

(require vraid/array
         ffi/unsafe)

(struct-array tile-climate-data
              ([snow _float]
               [sunlight _float]
               [temperature _float]
               [humidity _float]
               [precipitation _float]))

(struct-array edge-climate-data
              ([river-flow _float]
               [air-flow _float]))
