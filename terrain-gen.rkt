#lang racket

(require "terrain-dsl.rkt")

(provide (all-defined-out))

(define ((file->algorithm grids) file)
  (let* ([value (file->value file)])
    ; ensures that the algorithm is valid
    (((eval-terrain-function value) "") (grids 0))
    value))

(define (load-algorithms to-algorithm directory)
  (let ([files (filter (lambda (s)
                         (let ([split (string-split (path->string s) ".")])
                           (and (= 2 (length split))
                                (string=? "txt" (second split)))))
                       (directory-list directory))])
    (foldl (lambda (file hash)
             (let* ([name (string->symbol (first (string-split (path->string file) ".")))]
                    [path (build-path directory file)]
                    [value (to-algorithm path)])
               (hash-set hash name value)))
           #hash()
           files)))
