#lang racket

(require racket/gui/base)

(provide edit-field%)

(define edit-field%
  (class text-field%
    (init-field value
                value->string)
    (super-new [init-value (value->string (value))]
               [callback (λ (edit event)
                           (update))])
    (inherit get-value
             set-value)
    (define/public (update/value v)
      (set-value (value->string v)))
    (define/public (update)
      (update/value (value)))))
