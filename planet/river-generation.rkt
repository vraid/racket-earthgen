#lang typed/racket

(provide set-river-directions!)

(require "planet.rkt"
         racket/promise
         vraid/flow
         vraid/types)

(: set-river-directions! (planet -> Void))
(define (set-river-directions! planet)
  (for ([n (corner-count planet)])
    ((corner-data-river-direction-set!
      (planet-corner planet)) n (lowest-corner-direction planet n)))
  (void))

; direction is -1 if no neighbouring corner has lower elevation
(: lowest-corner-direction (planet Integer -> Fixnum))
(define (lowest-corner-direction planet n)
  (let* ([elevation (lambda: ([n : Integer])
                      (corner-elevation planet n))]
         [index/elevation (lambda: ([i : Fixnum])
                            (cons i (elevation
                                     (corner-corner planet n i))))]
         [indices/elevation (map index/elevation
                                 '(0 1 2))])
    (car (argmin (lambda: ([p : fix-float-pair])
                   (cdr p))
                 (cons (cons -1 (elevation n))
                       indices/elevation)))))

(define-type fix-float-pair (Pair Fixnum Flonum))

(: river-sources (planet -> (Integer -> integer-list)))
(define (river-sources planet)
  (let ([ls (build-vector (corner-count planet)
                          (lambda: ([n : Integer])
                            (delay (filter (river-flows-to? planet n)
                                           (grid-corner-corner-list planet n)))))])
    (lambda (n)
      (force (vector-ref ls n)))))
