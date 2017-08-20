#lang racket

(provide (all-defined-out))

(define (clear-compiled-directories dir)
  (let* ([join (curry build-path dir)]
         [dirs (filter (λ (a)
                         (and (directory-exists? (join a))
                              (not (equal? a (string->path ".git")))))
                       (directory-list dir))])
    (for ([a dirs])
      (let ([joined (join a)]
            [traverse (if (equal? a (string->path "compiled"))
                          delete-directory/files
                          clear-compiled-directories)])
        (traverse joined)))))
