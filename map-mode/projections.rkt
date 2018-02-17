#lang typed/racket

(require "../planet/math/projection.rkt")

(provide (all-defined-out))

(struct projection
  ([spherical->projected : (-> FlVector (-> FlVector FlVector))]
   [projected->spherical : (-> FlVector (Option FlVector))]))

(struct named-projection
  ([id : Symbol]
   [projection : projection]))

(define orthographic-projection
  (projection (λ ([a : FlVector])
                identity)
              orthographic->spherical))

(define hammer-projection
  (projection spherical->hammer
              hammer->spherical))

(: projections (Listof named-projection))
(define projections
  (list (named-projection 'orthographic orthographic-projection)
        (named-projection 'hammer hammer-projection)))

(: projection-list (Listof Symbol))
(define projection-list
  (map named-projection-id projections))

(define projection-hash
  (foldl (λ ([p : named-projection]
             [h : (HashTable Symbol projection)])
           (hash-set h (named-projection-id p) (named-projection-projection p)))
         (ann (hash) (HashTable Symbol projection))
         projections))
