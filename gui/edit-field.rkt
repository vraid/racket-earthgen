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
                               (begin
                                 (set! current-val (string->value (get-value)))
                                 (when (and on-enter (equal? 'text-field-enter (send event get-event-type)))
                                   (and-call on-enter current-val))
                                 (and-call on-change current-val))))])
    (inherit get-value
             set-value)
    (: current-val (Option a))
    (define current-val (string->value (get-value)))
    (: current-value (-> (Option a)))
    (define/public (current-value)
      current-val)
    (: update/value (a -> Any))
    (define/public (update/value v)
      (set! current-val v)
      (set-value (value->string v))
      (and-call on-change v))
    (: update (-> Any))
    (define/public (update)
      (update/value (value)))))
