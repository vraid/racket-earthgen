#lang typed/racket

(provide planet-handler%)

(require typed/racket/class
         vraid/math
         vraid/flow
         vraid/types
         vraid/util
         "../planet/planet.rkt"
         "../planet/planet-generation.rkt")

(define-type full-planet planet-climate)
(define-type maybe-full-planet (maybe full-planet))
(define-type planet-vector (Vectorof full-planet))

(define empty-planet-geometry
  (planet-geometry/kw
   #:grid (n-grid 0)
   #:axis default-axis
   #:radius 0.0
   #:tile (tile-geometry-data
           (lambda ([n : integer])
             0.0))))

(define empty-planet-water
  (let ([fl-zero (lambda ([n : integer])
                   0.0)]
        [void-fl-set (lambda ([n : integer]
                              [value : flonum])
                       (void))])
    (planet/sea-level 
     0.0
     (planet-terrain/kw
      #:planet-geometry empty-planet-geometry
      #:tile (tile-terrain-data fl-zero void-fl-set)
      #:corner (corner-terrain-data fl-zero void-fl-set)))))

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
                [set-status : (String -> Void) (lambda (s) (void))])
    (inherit earliest latest earlier later at-current-index element-count set-vec! get-vec index vector-from-index)
    (: terrain-func (-> planet-terrain))
    (define terrain-func (thunk empty-planet-water))
    (: terrain planet-terrain)
    (define terrain empty-planet-water)
    (: climate-func (planet-climate -> planet-climate))
    (define climate-func identity)
    (: current (-> (maybe planet-terrain)))
    (define/public (current)
      (if-let ([p (at-current-index)])
        p
        (if terrain terrain empty-planet-water)))
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
    (: reset/climate ((-> (planet-climate -> planet-climate)) (-> full-planet) -> Void))
    (define/public (reset/climate f initial)
      (work "generating climate"
            (thunk
             (set! climate-func (f))
             (set-vec! (vector (initial))))))
    (define/public (get-terrain)
      terrain)
    (define/public (ready?)
      (not working?))
    (: work (String (-> Any) -> Void))
    (define work
      (lambda ([status : String]
               [f : (-> Any)])
        (unless working?
          (set-status status)
          (set! working? #t)
          (f)
          (set! working? #f)
          (set-status "ready"))))
    (: terrain/scratch ((-> planet-terrain) -> Void))
    (define/public (terrain/scratch f)
      (work "generating terrain"
            (thunk
             (reset/terrain f))))
    (: terrain/modify ((planet-terrain -> planet-terrain) -> Void))
    (define/public (terrain/modify f)
      (work "generating terrain"
            (thunk
             (reset/terrain (thunk (f (terrain-func)))))))
    (: add/tick (-> Void))
    (define/public (add/tick)
      (let ([climate (current)])
        (when (planet-climate? climate)
          (work "turning world"
                (thunk
                 (set-vec!
                  (vector-append
                   (vector (climate-func climate))
                   (vector-from-index))))))))))
