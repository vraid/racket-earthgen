#lang typed/racket

(require vraid/color)

(provide (all-defined-out))

(define color-yellow (flcolor3 1.0 1.0 0.0))
(define color-red (flcolor3 1.0 0.0 0.0))

(define color-undefined
  (flcolor3 0.7 0.7 0.7))

(define color-neutral
  (flcolor3 0.95 0.95 0.85))

(define color-neutral-water
  (flcolor3 0.0 0.0 0.5))

(define snow-color (flcolor3 0.9 0.9 0.9))
