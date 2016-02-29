#lang racket

(provide (all-defined-out))

(require vraid/flow
         "planet/grid.rkt")

(struct mouse-state
  (moving? down? down-x down-y)
  #:mutable)

(define (mouse-input canvas control planet-handler update-tile-panel current-mouse-state event)
  (let ([x (send event get-x)]
        [y (send event get-y)])
    (if (send event button-up? 'left)
        (begin
          (unless (mouse-state-moving? current-mouse-state)
            (and-let* ([planet (send planet-handler current)]
                       [tile (grid-closest-tile planet (send control get-coordinates planet x y))])
                      (begin
                        (update-tile-panel planet tile)
                        (send canvas force-repaint))))
          (set-mouse-state-down?! current-mouse-state #f)
          (set-mouse-state-moving?! current-mouse-state #f))
        (if (mouse-state-down? current-mouse-state)
            (begin
              (when (not (and (= x (mouse-state-down-x current-mouse-state))
                              (= y (mouse-state-down-y current-mouse-state))))
                (set-mouse-state-moving?! current-mouse-state #t))
              (send control on-event event)
              (send canvas force-repaint))
            (if (send event button-down? 'left)
                (begin
                  (set-mouse-state-down-x! current-mouse-state x)
                  (set-mouse-state-down-y! current-mouse-state y)
                  (set-mouse-state-down?! current-mouse-state #t)
                  (send control on-event event))
                (void))))))
