#lang racket

(require "planet/grid-create.rkt"
         "terrain-dsl.rkt")

(provide file->algorithm
         load-algorithms)

(define (any? a)
  #t)

(define (file->algorithm file)
  (let* ([value (file->value file)])
    ; ensures that the algorithm is valid
    (((eval-terrain-function value) "") (list (n-grid 0)))
    value))

(define (load-algorithms directory)
  (let ([files (filter (lambda (s)
                         (let ([split (string-split (path->string s) ".")])
                           (and (= 2 (length split))
                                (string=? "txt" (second split)))))
                       (directory-list directory))])
    (foldl (lambda (file hash)
             (let* ([name (string->symbol (first (string-split (path->string file) ".")))]
                    [path (build-path directory file)]
                    [value (file->algorithm path)])
               (hash-set hash name value)))
           #hash()
           files)))
