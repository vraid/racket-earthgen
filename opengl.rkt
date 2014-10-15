#lang racket

(provide (all-defined-out))

(require ffi/vector
         ffi/cvector
         ffi/unsafe
         (planet stephanh/RacketGL:1:4/rgl))

(define uint-size 4)
(define byte-size 1)
(define float-size 4)
(define vertex-size (+ (* 3 float-size) (* 4 byte-size)))

(struct gl-vertex-buffer
  (handle
   data))

(struct gl-index-buffer
  (handle
   data))

(define-cstruct _gl-vertex
  ([x _float]
   [y _float]
   [z _float]
   [red _byte]
   [green _byte]
   [blue _byte]
   [alpha _byte]))

(define (generate-gl-buffer-handle)
  (let ((buffer (glGenBuffers 1)))
    (u32vector-ref buffer 0)))

(define (set-gl-vertex-buffer! buffer)
  (let ([handle (gl-vertex-buffer-handle buffer)]
        [vertices (gl-vertex-buffer-data buffer)])
    (glBindBuffer GL_ARRAY_BUFFER handle)
    (glBufferData GL_ARRAY_BUFFER (* vertex-size (cvector-length vertices)) (cvector-ptr vertices) GL_STATIC_DRAW)
    (glBindBuffer GL_ARRAY_BUFFER 0)))

(define (set-gl-index-buffer! buffer)
  (let* ([handle (gl-index-buffer-handle buffer)]
         [indices (gl-index-buffer-data buffer)])
    (glBindBuffer GL_ELEMENT_ARRAY_BUFFER handle)
    (glBufferData GL_ELEMENT_ARRAY_BUFFER (* uint-size (cvector-length indices)) (cvector-ptr indices) GL_DYNAMIC_DRAW)
    (glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0)))

(define (set-gl-ortho-projection left right bottom top near far)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho left right bottom top near far))

(define (set-gl-viewport left top width height)
  (glViewport left top width height))

(define (gl-rotate rotation)
  (apply glRotatef rotation))

(define (gl-translate translation)
  (apply glTranslatef translation))

(define (gl-cull-face side)
  (glEnable GL_CULL_FACE)
  (match side
    ('front (glCullFace GL_FRONT))
    ('back (glCullFace GL_BACK))
    ('both (glCullFace GL_FRONT_AND_BACK))
    (_ (void))))

(define (gl-clear color)
  (apply glClearColor color)
  (glClear GL_COLOR_BUFFER_BIT))

(define (gl-draw vertex-buffer index-buffer)
  (glFrontFace GL_CCW)
  (glShadeModel GL_SMOOTH)
  
  (glBindBuffer GL_ARRAY_BUFFER (gl-vertex-buffer-handle vertex-buffer))
  (glVertexPointer 3 GL_FLOAT vertex-size 0)
  (glColorPointer 4 GL_UNSIGNED_BYTE vertex-size (* 3 float-size))
  (glBindBuffer GL_ARRAY_BUFFER 0)
  
  (glEnableClientState GL_VERTEX_ARRAY)
  (glEnableClientState GL_COLOR_ARRAY)
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER (gl-index-buffer-handle index-buffer))
  
  (glDrawElements GL_TRIANGLES
                  (cvector-length (gl-index-buffer-data index-buffer))
                  GL_UNSIGNED_INT
                  0)
  
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0)
  (glDisableClientState GL_VERTEX_ARRAY)
  (glDisableClientState GL_COLOR_ARRAY))
