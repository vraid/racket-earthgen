#lang racket

(provide load-terrain)

(require "planet/heightmap.rkt")

(define-namespace-anchor a)

(define (load-terrain)
  (parameterize ([current-namespace (namespace-anchor->namespace a)])
    (load "terrain-gen.rkt")))
