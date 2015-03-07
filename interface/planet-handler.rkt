#lang typed/racket

(provide planet-handler%)

(require typed/racket/class
         vraid/math
         vraid/flow
         vraid/types
         vraid/util
         "../planet/planet.rkt")

(define-type maybe-planet (maybe planet))
(define-type planet-vector (Vectorof planet))

(define planet-stepper%
  (class object%
    (super-new)
    (field [vec : planet-vector (vector)])
    (: current-index integer)
    (define current-index 0)
    (: index (-> integer))
    (define/public (index) current-index)
    (: element-count (-> integer))
    (define/public (element-count) (vector-length vec))
    (: at (integer -> maybe-planet))
    (define at
      (lambda: ([n : integer])
        (if ((index-within-range? 0 (element-count)) n)
            (begin
              (set! current-index n)
              (vector-ref vec n))
            #f)))
    (: step-by (integer -> maybe-planet))
    (define step-by
      (lambda: ([n : integer])
        (at (+ current-index n))))
    (: set-vec! (planet-vector -> Void))
    (define/public (set-vec! v)
      (set! vec v)
      (set! current-index 0))
    (: get-vec (-> planet-vector))
    (define/public (get-vec)
      vec)
    (: current (-> maybe-planet))
    (define/public (current)
      (at current-index))
    (: later (-> maybe-planet))
    (define/public (later)
      (step-by -1))
    (: earlier (-> maybe-planet))
    (define/public (earlier)
      (step-by 1))
    (: latest (-> maybe-planet))
    (define/public (latest)
      (at 0))
    (: earliest (-> maybe-planet))
    (define/public (earliest)
      (at (- (element-count) 1)))))

(define planet-handler%
  (class planet-stepper%
    (super-new)
    (init-field [max-elements : Integer 24]
                [work-start : (String -> Void) (lambda (s) (void))]
                [work-end : (String -> Void) (lambda (s) (void))])
    (inherit earliest latest earlier later current element-count set-vec! get-vec index)
    (: terrain-func (-> planet))
    (define terrain-func (thunk empty-planet))
    (: working? Boolean)
    (define working? #f)
    (: reset/planet (planet -> planet))
    (define reset/planet
      (lambda: ([p : planet])
        (set-vec! (vector p))
        p))
    (define/public (ready?)
      (not working?))
    (: work ((-> Any) -> Void))
    (define work
      (lambda: ([f : (-> Any)])
        (unless working?
          (work-start "")
          (set! working? #t)
          (f)
          (set! working? #f)
          (work-end ""))))
    (: terrain/scratch ((-> planet) -> Void))
    (define/public (terrain/scratch f)
      (work (thunk
             (set! terrain-func f)
             (reset/planet (f)))))
    (: terrain/modify ((planet -> planet) -> Void))
    (define/public (terrain/modify f)
      (work (thunk
             (set! terrain-func (thunk
                                 (f (terrain-func))))
             (and-let ([p (earliest)])
               (reset/planet (f p))))))
    (: climate/scratch ((planet -> planet) -> Void))
    (define/public (climate/scratch f)
      (work (thunk
             (let* ([p (earliest)]
                    [p (if p p (terrain-func))])
               (reset/planet (f p))))))
    (: vector-from-index (-> planet-vector))
    (define (vector-from-index)
      (vector-drop (get-vec) (index)))
    (: climate/add ((planet -> planet) -> Void))
    (define/public (climate/add f)
      (work (thunk
             (and-let ([p (current)])
               (set-vec! (vector-append
                          (vector (f p))
                          (vector-take-at-most
                           (vector-from-index)
                           (- max-elements 1))))))))))
