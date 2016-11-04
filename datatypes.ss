(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vals (list-of box?))
    (env environment?))
  (recursively-extended-env-record
    (proc-names (list-of symbol?))
    (idss (list-of (pairs-of symbol?)))
    (bodiess (list-of (list-of expression?)))
    (env environment?)))


;; Parsed expression datatypes

(define-datatype expression expression?
  ;; for numbers and quoted things
  ;; to find quoted things, they are a list starting with quote
  [lit-exp
    (val is-literal?)]
  ;; for defined variables
  [var-exp
    (id symbol?)]
  ;; lambda has a few cases
  ;; (lambda (vars) bodies)
  ;; (lambda var bodies)
  ;; (lambda (first.rest) bodies)
  [lambda-exp-list (id (list-of symbol?)) (body (list-of expression?))]
  [lambda-exp-sym (id symbol?) (body (list-of expression?))]
  [lambda-exp-improper (id pair?) (body (list-of expression?))]
  ;; 
  [app-exp
    (rator expression?)
    (rands (list-of expression?))]
  ;; going to be handled as 
  [let-exp
    (var (list-of symbol?))
    (exp (list-of expression?))
    (body (list-of expression?))]
  [let*-exp
    (var (list-of symbol?))
    (exp (list-of expression?))
    (body (list-of expression?))]
  [letrec-exp
    (proc-names (list-of symbol?))
    (idss (list-of (pairs-of symbol?)))
    (bodiess (list-of (list-of expression?)))
    (letrec-bodies (list-of expression?))]
  [namedlet-exp
    (name symbol?)
    (var (list-of symbol?))
    (exp (list-of expression?))
    (body (list-of expression?))]
  [set!-exp
    (var symbol?)
    (val expression?)]
  [if-exp
    (con expression?)
    (thn expression?)
    (els (lambda (x) (or (expression? x) (null? x))))]
  [define-exp
    (var symbol?)
    (val expression?)])


                                        ; datatype for procedures.  At first there is only one
                                        ; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
    (name symbol?)]
  [closure
    (args (list-of symbol?))
    (body (list-of expression?))
    (env environment?)]
  [closure-alt
    (arg symbol?)
    (body (list-of expression?))
    (env environment?)]
  [closure-pair
    (arg (pairs-of symbol?))
    (body (list-of expression?))
    (env environment?)])

                                        ; ('prim-proc name)
                                        ; ('closure (args) (body) env)

(define make-prim
  (lambda (name)
    (list 'prim-proc name)))

(define make-closure
  (lambda (args body env)
    (list 'closure args body env)))

(define is-prim?
  (lambda (prim)
    (and (eqv? 'prim-proc (car prim))
         (symbol? (cadr prim))
         (eq? (length prim) 2))))

(define is-closure?
  (lambda (closure)
    (and (eqv? 'closure (car closure))
         (list? (cadr closure))
         (list? (caddr closure))
         (environment? (cadddr closure))
         (eq? 4 (length closure)))))

(define is-proc-val?
  (lambda (proc)
    (or (is-prim? proc) (is-closure? proc))))



;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype kontinuation kontinuation?
  [eval-rands-k
    (env environment?)
    (rands (list-of expression?))]
  [app-rator-k
    (env environment?)
    (rands (list-of expression?))]
  [app-rands-k
    (env environment?)
    (rator proc-val?)]
  [car-reverse-k]
  [map-k
    (proc proc-val?)
    (rands (list-of expression?))]
  [define-k]
  [if-k
    (thn expression?)
    (els expression?)
    (env environment?)]
  [set!-k
    (env environment?)
    (var symbol?)]
  [init-env-k]
  [reset-env-k])

(define apply-k
  (lambda (k v)
    (if (kontinuation? k)
      (cases kontinuation k
        [eval-rands-k (env rands)
          (if (null? rands)
              (list v)
              (cons v (eval-rands rands env '())))]
        [app-rator-k (env rands)
          (eval-rands rands env (app-rands-k env v))]
        [app-rands-k (env rator)
          (apply-proc rator v '())]
        [car-reverse-k ()
          (car (reverse v))]
        [map-k (proc rands)
          (if (null? rands)
            (cons v (apply-proc proc (list (car rands)) '()))
            (cons v (apply-proc proc (list (car rands)) (map-k proc (cdr rands)))))]
        [define-k ()
          (set-car! (cddr init-env) (append (caddr init-env) (list (box v))))]
        [if-k (thn els env)
          (if v
              (eval-exp thn env '())
              (if (null? els)
                  (void)
                  (eval-exp els env '())))]
        [set!-k (env var)
          (set-ref!
            (apply-env-ref env var
              (lambda (v) v)
              (lambda () (apply-env-ref init-env var
                           (lambda (x) x) 
                           (lambda () (eopl:error 'apply-env
                                        "variable not found in environment: ~s" var)))))
            v)]
        [init-env-k ()
          (extend-env *prim-proc-names* v (empty-env))]
        [reset-env-k ()
          (set! init-env (extend-env *prim-proc-names* v (empty-env)))])
      v)))
