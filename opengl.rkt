#lang racket

(provide _gl-vertex
         make-gl-vertex
         set-gl-vertex-data
         set-gl-index-data
         set-gl-ortho-projection
         rotate-gl
         set-gl-viewport
         draw-gl)

(require ffi/vector
         ffi/cvector
         ffi/unsafe
         (planet stephanh/RacketGL:1:4/rgl))

(define uint-size 4)
(define byte-size 1)
(define float-size 4)
(define vertex-size (+ (* 3 float-size) (* 4 byte-size)))

(define-cstruct _gl-vertex
  ([x _float]
   [y _float]
   [z _float]
   [red _byte]
   [green _byte]
   [blue _byte]
   [alpha _byte]))

(define vertex-data #f)
(define index-data #f)

(define vertex-buffer #f)
(define index-buffer #f)

(define (get-buffer-name)
  (let ((buffer (glGenBuffers 1)))
    (u32vector-ref buffer 0)))  

(define (set-gl-vertex-data vertices)
  (set! vertex-data vertices)
  (when (not vertex-buffer) (set! vertex-buffer (get-buffer-name)))
  (glBindBuffer GL_ARRAY_BUFFER vertex-buffer)
  (glBufferData GL_ARRAY_BUFFER (* vertex-size (cvector-length vertex-data)) (cvector-ptr vertex-data) GL_STATIC_DRAW)
  (glBindBuffer GL_ARRAY_BUFFER 0))

(define (set-gl-index-data indices)
  (set! index-data indices)
  (when (not index-buffer) (set! index-buffer (get-buffer-name)))
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER index-buffer)
  (glBufferData GL_ELEMENT_ARRAY_BUFFER (* uint-size (cvector-length index-data)) (cvector-ptr index-data) GL_DYNAMIC_DRAW)
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0))

(define (rotate-gl rotation)
  (apply glRotatef rotation))

(define (set-gl-ortho-projection left right bottom top near far)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho left right bottom top near far))

(define (set-gl-viewport left top width height)
  (glViewport left top width height))

(define (draw-gl)
  (begin
    (glFrontFace GL_CCW)
    (glEnable GL_CULL_FACE)
    (glCullFace GL_BACK)
    (glClearColor 0.0 0.0 0.0 0.0)
    (glClear GL_COLOR_BUFFER_BIT)
    (glShadeModel GL_SMOOTH)
    
    (glBindBuffer GL_ARRAY_BUFFER vertex-buffer)
    (glVertexPointer 3 GL_FLOAT vertex-size 0)
    (glColorPointer 4 GL_UNSIGNED_BYTE vertex-size (* 3 float-size))
    (glBindBuffer GL_ARRAY_BUFFER 0)
    
    (glEnableClientState GL_VERTEX_ARRAY)
    (glEnableClientState GL_COLOR_ARRAY)
    (glClear GL_COLOR_BUFFER_BIT)
    (glBindBuffer GL_ELEMENT_ARRAY_BUFFER index-buffer)
    
    (glDrawElements GL_TRIANGLES
                    (cvector-length index-data)
                    GL_UNSIGNED_INT
                    0)
    
    (glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0)
    (glDisableClientState GL_VERTEX_ARRAY)
    (glDisableClientState GL_COLOR_ARRAY)))
