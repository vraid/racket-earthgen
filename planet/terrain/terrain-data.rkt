#lang racket

(provide (all-defined-out))

(require vraid/array
         ffi/unsafe)

(struct-array tile-terrain-data
              ([elevation _float]))

(struct-array corner-terrain-data
              ([elevation _float]))
