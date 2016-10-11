; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

(define contains
  (lambda (ls val)
    (find (lambda (x) (eqv? x val)) ls)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
				(apply-env init-env id; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
		          "variable not found in environment: ~s"
			   id)))] 
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [if-exp (con thn els)
        (if (eval-exp con env)
          (eval-exp thn env)
          (if (not (null? els))
            (eval-exp els env)))]
      [let-exp (var exp body)
        (let ([extended-env
          (extend-env var (map (lambda (x) (eval-exp x env)) exp) env)])
          (eval-rands body extended-env))]
      [let*-exp (var exp body) ; Same as let because our modified map guarantees left->right
        (let ([extended-env
          (extend-env var (map (lambda (x) (eval-exp x env)) exp) env)])
          (eval-rands body extended-env))]
      [letrec-exp (var exp body)
        (let ([extended-env
          (extend-env var (map (lambda (x) (eval-exp x extended-env)) exp) env)])
          (eval-rands body extended-env))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define add1
  (lambda (x)
    (+ x 1)))
(define sub1
  (lambda (x)
    (- x 1)))
(define =
  (lambda (x y)
    (eqv? x y)))

(define *prim-proc-names* '(+ - * / add1 sub1 cons = zero? not < > <= >= != car cdr list null?
  assq eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector
  vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline caar cadr cdar cddr
  caaar caadr cadar caddr cdaar cdadr cddar cdddr))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (cond prim-proc
      [(contains *prim-proc-names* prim-proc)
        (apply prim-proc args)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))










