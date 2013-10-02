#lang racket

(require "planet.rkt")

(define (planet-tile-land?) #f)
(define (planet-tile-water?) #f)
(define (planet-tile-coast?) #f)

(define (planet-corner-land?) #f)
(define (planet-corner-water?) #f)
(define (planet-corner-coast?) #f)

(define (planet-edge-land?) #f)
(define (planet-edge-water?) #f)
(define (planet-edge-coast?) #f)
