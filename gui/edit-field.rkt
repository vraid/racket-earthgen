#lang typed/racket

(provide edit-field%)

(require typed/racket/gui/base
         vraid/flow)

(define edit-field%
  (class text-field%
    #:forall (a)
    (init-field [value : (-> a)]
                [value->string : (a -> String)]
                [string->value : (String -> (Option a))]
                [on-change : (Option ((Option a) -> Any)) #f]
                [on-enter : (Option ((Option a) -> Any)) #f]
                [read-only? : Boolean #f])
    (super-new [init-value (value->string (value))]
               [callback (lambda (edit event)
                           (if read-only?
                               (update)
                               (let ([v (string->value (get-value))])
                                 (when (and on-enter (equal? 'text-field-enter (send event get-event-type)))
                                   (call-when on-enter v))
                                 (call-when on-change v))))])
    (inherit get-value
             set-value)
    (: update (-> Any))
    (define/public (update)
      (let ([v (value)])
        (set-value (value->string v))
        (call-when on-change v)))))
