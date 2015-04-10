#lang racket

(provide (all-defined-out))

(require vraid/array
         ffi/unsafe)

(struct-array tile-water-data
              ([water-level _float]))

(struct-array corner-water-data
              ([river-direction _int]))
