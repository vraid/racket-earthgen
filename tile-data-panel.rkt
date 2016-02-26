#lang racket

(provide tile-data-panel)

(require racket/gui/base
         "gui/edit-panel.rkt"
         "planet/planet.rkt")

(define (tile-data-panel parent)
  (let* ([height 30]
         [label-width 100]
         [general-panel (new vertical-panel%
                             [parent parent]
                             [stretchable-height #f])]
         [general (edit-panel general-panel height label-width)]
         [terrain-panel (new vertical-panel%
                             [parent parent]
                             [stretchable-height #f])]
         [terrain (edit-panel terrain-panel height label-width)]
         [climate-panel (new vertical-panel%
                             [parent parent]
                             [stretchable-height #f])]
         [climate (edit-panel climate-panel height label-width)]
         [id-edit (general "tile id")]
         [elevation-edit (terrain "elevation")]
         [temperature-edit (climate "temperature")]
         [absolute-humidity-edit (climate "absolute humidity")]
         [relative-humidity-edit (climate "relative humidity")]
         [precipitation-edit (climate "precipitation")]
         [aridity-edit (climate "aridity")])
    (lambda (planet tile)
      (let* ([terrain (planet-terrain? planet)]
             [climate (planet-climate? planet)])
        (send general-panel show tile)
        (send terrain-panel show (and tile terrain))
        (send climate-panel show (and tile climate))
        (when tile
          (let ([link (lambda (panel get) (send panel link get (thunk* #f)))]
                [tile-number->string (lambda (f) (thunk (number->string (f planet tile))))])
            (link id-edit (thunk (number->string tile)))
            (when terrain
              (link elevation-edit
                    (tile-number->string tile-elevation)))
            (when climate
              (link temperature-edit
                    (tile-number->string tile-temperature))
              (link absolute-humidity-edit
                    (tile-number->string tile-humidity))
              (link relative-humidity-edit
                    (thunk (number->string (relative-humidity (tile-temperature planet tile)
                                                              (tile-humidity planet tile)))))
              (link precipitation-edit
                    (tile-number->string tile-precipitation))
              (link aridity-edit
                    (thunk (number->string (aridity (tile-temperature planet tile)
                                                    (tile-humidity planet tile))))))))))))
