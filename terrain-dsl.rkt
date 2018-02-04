#lang racket

(require vraid/random
         "planet/heightmap.rkt"
         "planet/heightmap/heightmap-create.rkt")

(provide eval-terrain-function)

(define ((eval-terrain-function ls) seed)
  (let* ([start-seed (string->seed seed)]
         [seed (primitive (lambda (n)
                            (make-seed (+ n start-seed))))])
    (eval (extend-environment base-environment '(seed) (list seed)) ls)))

(struct function
  (environment
   parameters
   body)
  #:transparent)

(struct primitive
  (function)
  #:transparent)

(struct syntax
  (function))

(struct binding
  (name value))

(define (create-heightmap seed base-level amplitude alpha beta beta-change)
  (heightmap-create
   (heightmap-parameters/kw
    #:seed seed
    #:base-level base-level
    #:amplitude amplitude
    #:alpha alpha
    #:beta beta
    #:beta-change beta-change)))

(define (error msg stx)
  (raise (cons msg stx)))

(define (check-length n ls)
  (unless (= n (length ls))
    (error "length mismatch" ls)))

(define (in-environment environment symbol)
  (if (empty? environment)
      (error "unbound variable " symbol)
      (let ([b (first environment)])
        (if (eq? (binding-name b) symbol)
            (binding-value b)
            (in-environment (rest environment) symbol)))))

(define (extend-environment environment names values)
  (foldl (lambda (name value env)
           (cons (binding name value) env))
         environment
         names
         values))

(define (eval environment syntax)
  (if (list? syntax)
      (eval-list environment syntax)
      (if (symbol? syntax)
          (in-environment environment syntax)
          syntax)))

(define (eval-list environment ls)
  (let ([f (eval environment (first ls))]
        [r (rest ls)])
    ((cond
       [(syntax? f) eval-syntax]
       [(function? f) eval-function]
       [(primitive? f) eval-primitive]
       [else (raise f)]) environment f r)))

(define (eval-values environment ls)
  (map (curry eval environment) ls))

(define (eval-syntax environment syntax body)
  ((syntax-function syntax) environment body))

(define (eval-function environment function args)
  (let ([args (eval-values environment args)])
    (apply call-function function args)))

(define (eval-primitive environment primitive args)
  (let ([args (eval-values environment args)])
    (apply (primitive-function primitive) args)))

(define (eval-lambda environment syntax)
  (check-length 2 syntax)
  (let ([parameters (first syntax)]
        [body (second syntax)])
    (function environment parameters body)))

(define (eval-let environment syntax)
  (check-length 2 syntax)
  (let* ([bindings (first syntax)]
         [body (second syntax)]
         [names (map first bindings)]
         [values (map second bindings)]
         [environment (foldl (lambda (name value env)
                               (cons (binding name (eval env value)) env))
                             environment
                             names
                             values)])
    (eval environment body)))

(define (eval-quote environment syntax)
  syntax)

(define (eval-if environment syntax)
  (check-length 3 syntax)
  (let* ([condition (eval environment (first syntax))])
    (eval environment ((if condition second third) syntax))))

(define (eval-map environment syntax)
  (let* ([n (length syntax)])
    (when (<= n 1)
      (error "bad syntax" syntax))
    (let* ([syntax-rest (eval-values environment syntax)]
           [function (first syntax-rest)]
           [arguments (rest syntax-rest)]
           [argcount (length arguments)]
           [call (if (function? function)
                     (curry call-function function)
                     (primitive-function function))])
      (if (<= argcount 1)
          (apply heightmap-map call arguments)
          (apply heightmap-map* call arguments)))))

(define (eval-noisemap name parameters function)
  (lambda (environment syntax)
    (let* ([bindings syntax]
           [names (map first bindings)]
           [values (map second bindings)]
           [h (foldl (lambda (name value h)
                       (hash-set h name (eval environment value)))
                     #hash()
                     names
                     values)])
      (unless (= (length (hash-keys h)) (length parameters))
        (error (string-append "bad parameters to " name) syntax))
      (for ([par parameters])
        (unless (hash-has-key? h par)
          (error (string-append "missing parameter: " (symbol->string par)) syntax)))
      (let* ([arguments (map (lambda (name)
                               (hash-ref h name))
                             parameters)])
        (apply function arguments)))))

(define eval-heightmap
  (eval-noisemap "heightmap"
                 '(seed base-level amplitude divergence)
                 (lambda (seed base-level amplitude divergence)
                   (let ([beta-change (- 1.0 (/ 1.0 divergence))])
                     (heightmap-create
                      (heightmap-parameters/kw
                       #:seed seed
                       #:base-level base-level
                       #:amplitude amplitude
                       #:alpha 1.0
                       #:beta 1.0
                       #:beta-change beta-change))))))

(define eval-noise
  (eval-noisemap "noise"
                 '(seed base-level persistence)
                 (lambda (seed base-level persistence)
                   (heightmap-create
                    (heightmap-parameters/kw
                     #:seed seed
                     #:base-level base-level
                     #:amplitude 1.0
                     #:alpha (- 1.0 persistence)
                     #:beta persistence
                     #:beta-change 1.0)))))

(define (call-function function . args)
  (let* ([environment (extend-environment (function-environment function)
                                          (function-parameters function)
                                          args)])
    (eval environment (function-body function))))

(define ((map-shorthand function) environment syntax)
  (unless (= 2 (length syntax))
    (error "bad syntax" syntax))
  (let* ([args (eval-values environment syntax)]
         [value (first args)])
    (apply heightmap-map (function value) (rest args))))

(define eval-raise (map-shorthand (curry +)))
(define eval-lower (map-shorthand (curryr -)))
(define eval-scale (map-shorthand (curry *)))

(define base-environment
  (list (binding 'heightmap (syntax eval-heightmap))
        (binding 'noise (syntax eval-noise))
        (binding 'lambda (syntax eval-lambda))
        (binding 'let (syntax eval-let))
        (binding 'quote (syntax eval-quote))
        (binding 'if (syntax eval-if))
        (binding 'map (syntax eval-map))
        (binding 'raise (syntax eval-raise))
        (binding 'lower (syntax eval-lower))
        (binding 'scale (syntax eval-scale))
        (binding '= (primitive =))
        (binding '< (primitive <))
        (binding '<= (primitive <=))
        (binding '> (primitive >))
        (binding '>= (primitive >=))
        (binding '+ (primitive +))
        (binding '- (primitive -))
        (binding '* (primitive *))
        (binding '/ (primitive /))
        (binding 'expt (primitive expt))
        (binding 'sqrt (primitive sqrt))
        (binding 'sign (primitive sgn))
        (binding 'abs (primitive abs))
        (binding 'min (primitive min))
        (binding 'max (primitive max))))
