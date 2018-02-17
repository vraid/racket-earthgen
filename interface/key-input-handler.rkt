#lang racket

(provide key-input-handler%)

(define key-input-handler%
  (class object%
    (super-new)
    (init-field zoom-in
                zoom-out
                exit)
    (define/public (on-key key-code)
      (match key-code
        ['escape (exit)]
        [#\z (zoom-in)]
        [#\x (zoom-out)]
        ['wheel-up (zoom-in)]
        ['wheel-down (zoom-out)]
        [_ (void)]))))
