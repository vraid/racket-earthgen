#lang typed/racket

(provide next-turn
         default-turn-parameters)

(require "planet-structs.rkt"
         "typed-struct-kw.rkt"
         "logistic.rkt"
         "if-let.rkt"
         "types.rkt")

(struct/kw: turn-parameters
            ())

(define default-turn-parameters
  (turn-parameters))

(: next-turn (turn-parameters planet -> planet))
(define (next-turn par p)
  (planet/kw
   #:grid p
   #:has-climate? (planet-has-climate? p)
   #:climate-parameters (planet-climate-parameters p)
   #:climate-variables (planet-climate-variables p)
   #:tile (planet-tile p)
   #:corner (planet-corner p)
   #:edge (planet-edge p)
   #:land-ratio (planet-land-ratio p)
   #:population (next-population p (planet-population p))))

(define population-growth-rate 1.03)

(: population-food-supply (planet integer population-type -> (maybe flonum)))
(define (population-food-supply p n type)
  (match type
    ('forager 1.0)
    (else #f)))

(define turn-time 1.0)

(define-type population-pair (Pairof population-type flonum))
(define-type population-list (Listof population-pair))

(: population-pairs (planet integer -> population-list))
(define (population-pairs p n)
  (: rec ((Listof population-type) population-list -> population-list))
  (define (rec types res)
    (if (empty? types)
        res
        (let ([key (first types)])
          (rec (rest types) (if-let* ([population (((planet-population p) n) key)]
                                      [food-supply (population-food-supply p n key)])
                                     (cons (cons key (logistic-absolute population-growth-rate population food-supply turn-time)) res)
                                     res)))))
  (rec (list 'forager 'pastoral 'agricultural) '()))

(: population-map (planet integer -> population-hash))
(define (population-map p n)
  (make-hash (population-pairs p n)))

(: next-population (planet population-access -> population-access))
(define (next-population p population)
  (let ([v (build-vector (tile-count p) (curry population-map p))])
    (lambda: ([n : Integer])
      (lambda: ([key : population-type])
        (hash-ref (vector-ref v n) key #f)))))
