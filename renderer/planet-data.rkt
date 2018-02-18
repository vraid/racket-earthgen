#lang typed/racket

(provide (all-defined-out))

(struct planet-data
  ([radius : Float]
   [tile-count : Integer]
   [tile-coordinates : (Integer -> FlVector)]
   [corner-coordinates : (Integer -> FlVector)]
   [tile-corner : (Integer Integer -> Integer)]
   [tile-edge : (Integer Integer -> Integer)]
   [tile-edge-count : (Integer -> Integer)]
   [river-flows-to? : (Integer -> (Integer -> Boolean))]
   [corner-river-flow : (Integer -> Float)]
   [edge-river-flow : (Integer -> Float)]))
