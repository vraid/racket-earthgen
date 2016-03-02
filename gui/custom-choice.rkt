#lang racket

(provide (all-defined-out))

(require racket/gui/base)

(define custom-choice%
  (class choice%
    (super-new)
    (inherit append
             clear)
    (define/public (set-choices choices)
      (define (rec ls)
        (unless (empty? ls)
          (begin
            (append (first ls))
            (rec (rest ls)))))
      (clear)
      (rec choices))))
