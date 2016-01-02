#lang typed/racket

(provide (all-defined-out))

(require "math.rkt"
         "color.rkt")

(struct gl-buffer
  ([set-vertex-coord! : (Integer flvector3 -> Void)]
   [set-vertex-color! : (Integer flcolor -> Void)]
   [set-index! : (Integer Integer -> Void)]
   [resize : (Integer Integer -> Void)]
   [bind : (-> Void)]
   [draw : (-> Void)]))
