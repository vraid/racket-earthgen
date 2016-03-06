#lang racket

(provide (all-defined-out))

(require racket/gui/base)

(define thunkf (thunk* #f))

(define edit-field%
  (class text-field%
    (init-field value
                value->string
                string->value
                [on-change thunkf]
                [on-enter thunkf]
                [read-only? #f])
    (super-new [init-value (value->string (value))]
               [callback (lambda (edit event)
                           (if read-only?
                               (update)
                               (when (or on-enter on-change)
                                 (let ([v (string->value (get-value))])
                                   (when (equal? 'text-field-enter (send event get-event-type))
                                     (on-enter v))
                                   (on-change v)))))])
    (inherit get-value
             set-value)
    (define/public (update)
      (let ([v (value)])
        (set-value (value->string v))
        (on-change v)))))
