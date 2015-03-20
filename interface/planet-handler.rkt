#lang typed/racket

(provide planet-handler%)

(require typed/racket/class
         vraid/math
         vraid/flow
         vraid/types
         vraid/util
         "../planet/planet.rkt")

(define-type full-planet planet-climate)
(define-type maybe-full-planet (maybe full-planet))
(define-type planet-vector (Vectorof full-planet))

(define empty-planet-geometry
  (planet-geometry/kw
   #:grid (n-grid 0)
   #:axis default-axis))

(define empty-planet-terrain
  (let ([fl-zero (lambda ([n : integer])
                   0.0)]
        [void-fl-set (lambda ([n : integer]
                              [value : flonum])
                       (void))]
        [int-1 (lambda ([n : integer])
                 -1)]
        [void-int-set (lambda ([n : integer]
                               [value : integer])
                        (void))])
    (planet-terrain/kw
     #:planet-geometry empty-planet-geometry
     #:sea-level 0.0
     #:tile (tile-terrain-data fl-zero void-fl-set)
     #:corner (corner-terrain-data fl-zero int-1 void-fl-set void-int-set)
     #:rivers '())))

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
    (: at (integer -> maybe-full-planet))
    (define at
      (lambda ([n : integer])
        (if ((index-within-range? 0 (element-count)) n)
            (begin
              (set! current-index n)
              (vector-ref vec n))
            #f)))
    (: step-by (integer -> maybe-full-planet))
    (define step-by
      (lambda ([n : integer])
        (at (+ current-index n))))
    (: set-vec! (planet-vector -> Void))
    (define/public (set-vec! v)
      (set! vec v)
      (set! current-index 0))
    (: get-vec (-> planet-vector))
    (define/public (get-vec)
      vec)
    (define/public (at-current-index)
      (at current-index))
    (: later (-> maybe-full-planet))
    (define/public (later)
      (step-by -1))
    (: earlier (-> maybe-full-planet))
    (define/public (earlier)
      (step-by 1))
    (: latest (-> maybe-full-planet))
    (define/public (latest)
      (at 0))
    (: earliest (-> maybe-full-planet))
    (define/public (earliest)
      (at (- (element-count) 1)))
    (: vector-from-index (-> planet-vector))
    (define/public (vector-from-index)
      (vector-drop (get-vec) (index)))))

(define planet-handler%
  (class planet-stepper%
    (super-new)
    (init-field [max-elements : Integer 24]
                [work-start : (String -> Void) (lambda (s) (void))]
                [work-end : (String -> Void) (lambda (s) (void))])
    (inherit earliest latest earlier later at-current-index element-count set-vec! get-vec index vector-from-index)
    (: terrain-func (-> planet-terrain))
    (define terrain-func (thunk empty-planet-terrain))
    (: terrain planet-terrain)
    (define terrain empty-planet-terrain)
    (: climate-func (planet-climate -> planet-climate))
    (define climate-func identity)
    (: current (-> (maybe planet-terrain)))
    (define/public (current)
      (if-let ([p (at-current-index)])
        p
        (if terrain terrain empty-planet-terrain)))
    (: working? Boolean)
    (define working? #f)
    (: reset/terrain ((-> planet-terrain) -> planet-terrain))
    (define reset/terrain
      (lambda ([f : (-> planet-terrain)])
        (set! terrain-func f)
        (let ([p (f)])
          (set! terrain p)
          (set-vec! (vector))
          p)))
    (: reset/climate ((planet-climate -> planet-climate) full-planet -> Void))
    (define/public reset/climate
      (lambda ([f : (planet-climate -> planet-climate)]
               [initial : full-planet])
        (set! climate-func f)
        (set-vec! (vector initial))))
    (define/public (get-terrain)
      terrain)
    (define/public (ready?)
      (not working?))
    (: work ((-> Any) -> Void))
    (define work
      (lambda ([f : (-> Any)])
        (unless working?
          (work-start "")
          (set! working? #t)
          (f)
          (set! working? #f)
          (work-end ""))))
    (: terrain/scratch ((-> planet-terrain) -> Void))
    (define/public (terrain/scratch f)
      (work (thunk
             (reset/terrain f))))
    (: terrain/modify ((planet-terrain -> planet-terrain) -> Void))
    (define/public (terrain/modify f)
      (work (thunk
             (reset/terrain (thunk (f (terrain-func)))))))
    (: add/tick (-> Void))
    (define/public (add/tick)
      (let ([climate (current)])
        (when (planet-climate? climate)
          (work
           (thunk
            (set-vec!
             (vector-append
              (vector (climate-func climate))
              (vector-from-index))))))))))
