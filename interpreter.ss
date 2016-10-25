;; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ;; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

;; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env env id; look up its value.
          (lambda (x) x) ; procedure to call if id is in the environment 
          (lambda () (apply-env init-env id 
                       (lambda (x) x) 
                       (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                                    "variable not found in environment: ~s" id)))))] 
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [if-exp (con thn els)
        (if (eval-exp con env)
            (eval-exp thn env)
            (if (not (null? els))
                (eval-exp els env)
                (void)))]
      [let-exp (var exp body)
        (let ([extended-env
                (extend-env var (map (lambda (x) (eval-exp x env)) exp) env)])
          (eval-bodies body extended-env))]
      [let*-exp (var exp body) ; Same as let because our modified map guarantees left->right
        (let ([extended-env
                (extend-env var (map (lambda (x) (eval-exp x env)) exp) env)])
          (eval-bodies body extended-env))]
      [letrec-exp (proc-names idss bodiess letrec-bodies)
        (eval-bodies letrec-bodies
          (extend-env-recursively proc-names idss bodiess env))]
      [lambda-exp-list (id body) ;; there is probably a way to clean this up a bit more than this
        (closure id body env)] ; but this will also work and I think it fits into the rest of the 
      [lambda-exp-sym (id body) ; assignments
        (closure-alt id body env)]
      [lambda-exp-improper (id body)
        (closure-pair id body env)]        
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;; evaluate the list of bodies, returning the last result
;; This works because our new map guarantees left->right

(define eval-bodies
  (lambda (bodies env)
    (car (reverse (eval-rands bodies env)))))

;;  Apply a procedure to its arguments.
;;  At this point, we only have primitive procedures.  
;;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (procargs body env)
        (let ([extended-env
                (extend-env procargs args env)])
          (eval-bodies body extended-env))]
      [closure-alt (procargs body env)
        (let ([extended-env
                (extend-env (list procargs) (list args) env)])
          (eval-bodies body extended-env))]
      [closure-pair (procargs body env)
        (let* ([mapped-vars
                (let loop ((var procargs) (args args) (newVar '()) (newArgs '()))
                  (cond [(and (null? args) (pair? var))
                         (error 'apply-proc "Attempt to apply bad args to: ~s" proc-val)]
                        [(pair? var) (loop (cdr var) (cdr args) (cons (car var) newVar) (cons (car args) newArgs))]
                        [else (list (reverse (cons var newVar)) (reverse (cons args newArgs)))]))]
               [extended-env (extend-env (car mapped-vars) (cadr mapped-vars) env)])
          (eval-bodies body extended-env))]
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

(define *prim-proc-names* '(+ - * / add1 sub1 cons = zero? not < > <= >= != car cdr list null? assq eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr apply map quotient list-tail append eqv?))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
    *prim-proc-names*   ;  a value (not an expression) with an identifier.
    (map prim-proc      
      *prim-proc-names*)
    (empty-env)))

;; Usually an interpreter must define each 
;; built-in procedure individually.  We are "cheating" a little bit.
;; Remember to add the name to the names defined in  *prim-proc-names*

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(quotient) (apply quotient args)]
      [(add1) (apply add1 args)]
      [(sub1) (apply sub1 args)]
      [(cons) (apply cons args)]
      [(=) (apply = args)]
      [(zero?) (apply zero? args)]
      [(not) (apply not args)]
      [(<) (apply < args)]
      [(>) (apply > args)]
      [(<=) (apply <= args)]
      [(>=) (apply >= args)]
      [(!=) (apply != args)]
      [(car) (apply car args)]
      [(cdr) (apply cdr args)]
      [(list) (apply list args)]
      [(null?) (apply null? args)]
      [(assq) (apply assq args)]
      [(eq?) (apply eq? args)]
      [(equal?) (apply equal? args)]
      [(atom?) (apply atom? args)]
      [(length) (apply length args)]
      [(list->vector) (apply list->vector args)]
      [(list?) (apply list? args)]
      [(pair?) (apply pair? args)]
      [(procedure?) (or (apply proc-val? args) (apply procedure? args))]
      [(vector->list) (apply vector->list args)]
      [(vector) (apply vector args)]
      [(make-vector) (apply make-vector args)]
      [(vector-ref) (apply vector-ref args)]
      [(vector?) (apply vector? args)]
      [(number?) (apply number? args)]
      [(symbol?) (apply symbol? args)]
      [(set-car!) (apply set-car! args)]
      [(set-cdr!) (apply set-cdr! args)]
      [(vector-set!) (apply vector-set! args)]
      [(display) (apply display args)]
      [(newline) (apply newline args)]
      [(caar) (apply caar args)]
      [(cadr) (apply cadr args)]
      [(cdar) (apply cdar args)]
      [(cddr) (apply cddr args)]
      [(caaar) (apply caaar args)]
      [(caadr) (apply caadr args)]
      [(cadar) (apply cadar args)]
      [(caddr) (apply caddr args)]
      [(cdaar) (apply cdaar args)]
      [(cdadr) (apply cdadr args)]
      [(cddar) (apply cddar args)]
      [(cdddr) (apply cdddr args)]
      ;; assignment 14
      [(apply) (apply (lambda x (apply-proc (car args) x)) (cadr args))]
      [(map) (map (lambda x (apply-proc (car args) x)) (cadr args))]
      ;; assignment 15
      [(list-tail) (apply list-tail args)]
      [(append) (apply append args)]
      [(eqv?) (apply eqv? args)]
      [else (error 'apply-prim-proc 
              "Bad primitive procedure name: ~s" 
              prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (letrec ([answer (top-level-eval (syntax-expand (parse-exp (read))))]
             [loop
               (lambda (val)
                 (cond
                   [(null? val) '()]
                   [(proc-val? val) '<interpreter-procedure>]
                   [(pair? val) (cons (loop (car val)) (loop (cdr val)))]
                   [else val]))])
      (eopl:pretty-print (loop answer))
      (newline))
    (rep)))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))










