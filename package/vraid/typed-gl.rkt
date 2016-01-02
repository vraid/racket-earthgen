#lang typed/racket

(require "gl-structs.rkt")

(provide (all-from-out "gl-structs.rkt")
         make-gl-buffer
         gl-cull-face)

(require/typed "opengl.rkt"
               [make-gl-buffer (Integer Integer -> gl-buffer)]
               [gl-cull-face ((U 'front 'back 'both 'none) -> Void)])
