#lang racket

(require racket/gui/base)

(provide (all-defined-out))

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
