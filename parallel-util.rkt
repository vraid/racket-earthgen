#lang racket

(require racket/future)

(provide vector-map!-parallel)

(define (vector-map!-parallel fn vec)
  (define fs (for/vector ([n (build-vector (vector-length vec) identity)])
               (future (lambda () (vector-set! vec n (fn (vector-ref vec n)))))))
  (begin
    (for/vector ([f fs]) (touch f))
    vec))
