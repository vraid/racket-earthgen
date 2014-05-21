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

(struct gl-buffer
  (handle
   data))

(define buffers (make-hash))

(define (get-gl-buffer id)
  (hash-ref buffers id #f))

(define-cstruct _gl-vertex
  ([x _float]
   [y _float]
   [z _float]
   [red _byte]
   [green _byte]
   [blue _byte]
   [alpha _byte]))

(define (generate-buffer-handle)
  (let ((buffer (glGenBuffers 1)))
    (u32vector-ref buffer 0)))

(define (set-gl-vertex-buffer! id vertices)
  (let* ([handle (if (not (get-gl-buffer id))
                     (generate-buffer-handle)
                     (gl-buffer-handle (get-gl-buffer id)))]
         [buffer (gl-buffer
                  handle
                  vertices)])
    (hash-set! buffers id buffer)
    (glBindBuffer GL_ARRAY_BUFFER handle)
    (glBufferData GL_ARRAY_BUFFER (* vertex-size (cvector-length vertices)) (cvector-ptr vertices) GL_STATIC_DRAW)
    (glBindBuffer GL_ARRAY_BUFFER 0)))

(define (set-gl-index-buffer! id indices)
  (let* ([handle (if (not (get-gl-buffer id))
                     (generate-buffer-handle)
                     (gl-buffer-handle (get-gl-buffer id)))]
         [buffer (gl-buffer
                  handle
                  indices)])
    (hash-set! buffers id buffer)
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
  
  (glBindBuffer GL_ARRAY_BUFFER (gl-buffer-handle (get-gl-buffer vertex-buffer)))
  (glVertexPointer 3 GL_FLOAT vertex-size 0)
  (glColorPointer 4 GL_UNSIGNED_BYTE vertex-size (* 3 float-size))
  (glBindBuffer GL_ARRAY_BUFFER 0)
  
  (glEnableClientState GL_VERTEX_ARRAY)
  (glEnableClientState GL_COLOR_ARRAY)
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER (gl-buffer-handle (get-gl-buffer index-buffer)))
  
  (glDrawElements GL_TRIANGLES
                  (cvector-length (gl-buffer-data (get-gl-buffer index-buffer)))
                  GL_UNSIGNED_INT
                  0)
  
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0)
  (glDisableClientState GL_VERTEX_ARRAY)
  (glDisableClientState GL_COLOR_ARRAY))
