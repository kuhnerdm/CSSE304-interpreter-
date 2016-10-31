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
    (pos number?)
    (layer (lambda (x) (or (number? x) (symbol? x))))]
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
    (var (pair-of number?))
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

(define-datatype dataref dataref?
  [isdata
    (data (lambda (x) (or (eqv? (car x) 'lit-exp) (proc-val? x))))]
  [isref
    (ref (pair-of number?))])



;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

