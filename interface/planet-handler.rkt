#lang racket

(provide planet-handler%)

(require vraid/math
         vraid/flow
         vraid/util)

(define vector-stepper%
  (class object%
    (super-new)
    (field [vec (vector)])
    (define current-index 0)
    (define/public (index) current-index)
    (define/public (element-count) (vector-length vec))
    (define (at n)
      (if ((index-within-range? 0 (element-count)) n)
          (begin
            (set! current-index n)
            (vector-ref vec n))
          #f))
    (define (step-by n)
      (at (+ current-index n)))
    (define/public (set-vec! v)
      (set! vec v)
      (set! current-index 0))
    (define/public (get-vec)
      vec)
    (define/public (current)
      (at current-index))
    (define/public (later)
      (step-by -1))
    (define/public (earlier)
      (step-by 1))
    (define/public (latest)
      (at 0))
    (define/public (earliest)
      (at (- (element-count) 1)))))

(define planet-handler%
  (class vector-stepper%
    (super-new)
    (init-field [max-elements 24]
                [work-start (thunk* #f)]
                [work-end (thunk* #f)])
    (inherit earliest latest earlier later current element-count set-vec! get-vec index)
    (define terrain-func #f)
    (define working? #f)
    (define (reset/planet p)
      (set-vec! (vector p))
      p)
    (define/public (ready?)
      (not working?))
    (define (work f)
      (unless working?
        (work-start)
        (set! working? #t)
        (f)
        (set! working? #f)
        (work-end)))
    (define/public (terrain/scratch f)
      (work (thunk
             (set! terrain-func f)
             (reset/planet (f)))))
    (define/public (terrain/modify f)
      (work (thunk
             (set! terrain-func (compose f terrain-func))
             (and-let ([p (earliest)])
               (reset/planet (f p))))))
    (define/public (climate/scratch f)
      (work (thunk
             (let* ([p (earliest)]
                    [p (if p p (terrain-func))])
               (reset/planet (f p))))))
    (define (vector-from-index)
      (vector-drop (get-vec) (index)))
    (define/public (climate/add f)
      (work (thunk
             (and-let ([p (current)])
               (set-vec! (vector-append
                          (vector (f p))
                          (vector-take-at-most
                           (vector-from-index)
                           (- max-elements 1))))))))))
