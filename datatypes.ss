
;; Parsed expression datatypes

(define-datatype expression expression?
  ;; for numbers and quoted things
  ;; to find quoted things, they are a list starting with quote
  [lit-exp
    (val (or2Type (lambda (x) (not (list? x))) (lambda (x) (eqv? (car x) 'quote))))]
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
    (var (list-of symbol?))
    (exp (list-of expression?))
    (body (list-of expression?))]
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
    (els expression?)])

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)])
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))