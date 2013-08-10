#lang racket
(require racket/gui/base)
(require sgl)
(require "logic.rkt")

(let*-values ([(screen-width screen-height) (get-display-size)]
              [(frame) (new frame% [label "earthgen"]
                                   [width screen-width]
                                   [height screen-height]
                                   [style '(no-resize-border no-caption no-system-menu)])]
              [(canvas) (new canvas% [parent frame])]
              [(draw-context) (send canvas get-dc)])
  (send frame show true))
