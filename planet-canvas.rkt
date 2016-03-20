#lang racket

(require vraid/opengl
         racket/gui/base)

(provide planet-canvas%)

(define planet-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (init-field milliseconds-between-frames
                resize-viewport
                on-key-event
                on-mouse-event
                paint)
    (define last-draw #f)
    (define (execute-paint)
      (thread
       (thunk
        (with-gl-context
         (thunk (paint)
                (swap-gl-buffers)))
        (set! last-draw (current-inexact-milliseconds)))))
    (define/override (on-paint)
      (when (or (not last-draw)
                (< milliseconds-between-frames
                   (- (current-inexact-milliseconds) last-draw)))
        (execute-paint)))
    (define/public (force-repaint)
      (execute-paint))
    (define/override (on-size width height)
      (resize-viewport width height)
      (with-gl-context
       (thunk
        (set-gl-viewport 0 0 width height))))
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (on-key-event key-code))
    (define/override (on-event event)
      (on-mouse-event event))
    (super-instantiate () (style '(gl)))))
