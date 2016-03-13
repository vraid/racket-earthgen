#lang typed/racket

(provide planet-handler%)

(require typed/racket/class
         "../planet/grid-base.rkt"
         "../planet/geometry-base.rkt")

(define planet-handler%
  (class object%
    (super-new)
    (init-field [set-status : (String -> Void) (lambda (s) (void))]
                [on-start : (-> Any) (thunk #f)]
                [on-change : (grid -> Void) (lambda (a) (void))])
    (: planet grid)
    (define planet empty-planet-geometry)
    (: current (-> grid))
    (define/public (current)
      planet)
    (: working? Boolean)
    (define working? #f)
    (define/public (ready?)
      (not working?))
    (: generate (String (-> grid) (grid -> Any) -> Void))
    (define/public (generate status f on-finish)
      (unless working?
        (set-status status)
        (set! working? #t)
        (on-start)
        (let ([p (f)])
          (set! planet p)
          (on-finish p)
          (on-change p)
          (set! working? #f))
        (set-status "ready")))))
