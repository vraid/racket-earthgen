#lang typed/racket

(provide edit-field%)

(require typed/racket/gui/base)

(define edit-field%
  (class text-field%
    #:forall (a)
    (init-field [value : (-> a)]
                [value->string : (a -> String)])
    (super-new [init-value (value->string (value))]
               [callback (lambda (edit event)
                           (update))])
    (inherit get-value
             set-value)
    (: update/value (a -> Any))
    (define/public (update/value v)
      (set-value (value->string v)))
    (: update (-> Any))
    (define/public (update)
      (update/value (value)))))
