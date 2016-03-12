#lang typed/racket

(provide (all-defined-out))

(require "color.rkt")

(struct gl-buffer
  ([set-vertex-coord! : (Integer FlVector -> Void)]
   [set-vertex-color! : (Integer flcolor -> Void)]
   [set-index! : (Integer Integer -> Void)]
   [resize : (Integer Integer -> Void)]
   [bind : (-> Void)]
   [draw : (-> Void)]))
