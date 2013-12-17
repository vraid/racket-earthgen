#lang racket

(require racket/future)

(provide vector-set!-parallel
         vector-map!-parallel)

(define (vector-set!-parallel fn vec)
  (define fs (build-vector
              (vector-length vec)
              (lambda (n)
                (future (lambda () (vector-set! vec n (fn n)))))))
  (begin
    (for/vector ([f fs]) (touch f))
    vec))


(define (vector-map!-parallel fn vec)
  (define fs (build-vector
              (vector-length vec)
              (lambda (n)
                (future (lambda () (vector-set! vec n (fn (vector-ref vec n))))))))
  (begin
    (for/vector ([f fs]) (touch f))
    vec))
