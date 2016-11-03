;; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ;; later we may add things that are not expressions.
      (eval-exp form (empty-env))))

;; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum) (apply-k k datum)]
      [var-exp (id)
        (apply-k k (apply-env env id; look up its value.
          (lambda (x) x) ; procedure to call if id is in the environment 
          (lambda () (apply-env init-env id 
                       (lambda (x) x) 
                       (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                                    "variable not found in environment: ~s" id))))))] 
      [app-exp (rator rands)
        (apply-k k (eval-exp rator env (app-rator-k env rands)))]
      [if-exp (con thn els)
        (if (eval-exp con env)
            (eval-exp thn env)
            (if (not (null? els))
                (eval-exp els env)
                (void)))]
      [letrec-exp (proc-names idss bodiess letrec-bodies)
        (eval-bodies letrec-bodies
          (extend-env-recursively proc-names idss bodiess env))]
      [lambda-exp-list (id body) ;; there is probably a way to clean this up a bit more than this
        (closure id body env)] ; but this will also work and I think it fits into the rest of the 
      [lambda-exp-sym (id body) ; assignments
        (closure-alt id body env)]
      [lambda-exp-improper (id body)
        (closure-pair id body env)]
      [set!-exp (var val)
        (set-ref!
          (apply-env-ref env var
            (lambda (v) v) ; success
            (lambda () (apply-env-ref init-env var
                         (lambda (x) x) 
                         (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                                      "variable not found in environment: ~s" id)))))
          (eval-exp val env))]
      [define-exp (var val)
        (set-car! (cdr init-env) (append (cadr init-env) (list var)))
        (set-car! (cddr init-env) (append (caddr init-env) (list (box (eval-exp val env)))))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env k)
    (if (null? rands)
      (apply-k k rands)
      (apply-k k (eval-exp (car rands) env (eval-rands-k env (cdr rands)))))))

;; evaluate the list of bodies, returning the last result
;; This works because our new map guarantees left->right

(define eval-bodies
  (lambda (bodies env k)
    (eval-rands bodies env (car-reverse-k k))))

;;  Apply a procedure to its arguments.
;;  At this point, we only have primitive procedures.  
;;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args k)]
      [closure (procargs body env)
        (let ([extended-env
                (extend-env procargs args env)])
          (eval-bodies body extended-env k))]
      [closure-alt (procargs body env)
        (let ([extended-env
                (extend-env (list procargs) (list args) env)])
          (eval-bodies body extended-env k))]
      [closure-pair (procargs body env)
        (let* ([mapped-vars
                 (let loop ((var procargs) (args args) (newVar '()) (newArgs '()))
                   (cond [(and (null? args) (pair? var))
                          (error 'apply-proc "Attempt to apply bad args to: ~s" proc-val)]
                         [(pair? var) (loop (cdr var) (cdr args) (cons (car var) newVar) (cons (car args) newArgs))]
                         [else (list (reverse (cons var newVar)) (reverse (cons args newArgs)))]))]
               [extended-env (extend-env (car mapped-vars) (cadr mapped-vars) env)])
          (eval-bodies body extended-env k))]
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

(define *prim-proc-names* '(list + - * / add1 sub1 cons = zero? not < > <= >= != car cdr null? assq eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr apply map quotient list-tail append eqv? newline display))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
    *prim-proc-names*   ;  a value (not an expression) with an identifier.
    (map prim-proc      
      *prim-proc-names*)
    (empty-env)))

(define reset-global-env
  (lambda () (set! init-env         ; for now, our initial global environment only contains 
          (extend-env            ; procedure names.  Recall that an environment associates
            *prim-proc-names*   ;  a value (not an expression) with an identifier.
            (map prim-proc *prim-proc-names*)
            (empty-env)))))

;; Usually an interpreter must define each 
;; built-in procedure individually.  We are "cheating" a little bit.
;; Remember to add the name to the names defined in  *prim-proc-names*

(define apply-prim-proc
  (lambda (prim-proc args k)
    (case prim-proc
      [(+) (apply-k k (apply + args))]
      [(-) (apply-k k (apply - args))]
      [(*) (apply-k k (apply * args))]
      [(/) (apply-k k (apply / args))]
      [(add1) (apply-k k (apply add1 args))]
      [(sub1) (apply-k k (apply sub1 args))]
      [(cons) (apply-k k (apply cons args))]
      [(=) (apply-k k (apply = args))]
      [(zero?) (apply-k k (apply zero? args))]
      [(not) (apply-k k (apply not args))]
      [(<) (apply-k k (apply < args))]
      [(>) (apply-k k (apply > args))]
      [(<=) (apply-k k (apply <= args))]
      [(>=) (apply-k k (apply >= args))]
      [(!=) (apply-k k (apply != args))]
      [(car) (apply-k k (apply car args))]
      [(cdr) (apply-k k (apply cdr args))]
      [(null?) (apply-k k (apply null? args))]
      [(assq) (apply-k k (apply assq args))]
      [(eq?) (apply-k k (apply eq? args))]
      [(equal?) (apply-k k (apply equal? args))]
      [(atom?) (apply-k k (apply atom? args))]
      [(length) (apply-k k (apply length args))]
      [(list->vector) (apply-k k (apply list->vector args))]
      [(list?) (apply-k k (apply list? args))]
      [(pair?) (apply-k k (apply pair? args))]
      [(procedure?) (apply-k k (apply procedure? args))]
      [(vector->list) (apply-k k (apply vector->list args))]
      [(vector) (apply-k k (apply vector args))]
      [(make-vector) (apply-k k (apply make-vector args))]
      [(vector-ref) (apply-k k (apply vector-ref args))]
      [(vector?) (apply-k k (apply vector? args))]
      [(number?) (apply-k k (apply number? args))]
      [(symbol?) (apply-k k (apply symbol? args))]
      [(set-car!) (apply-k k (apply set-car! args))]
      [(set-cdr!) (apply-k k (apply set-cdr! args))]
      [(vector-set!) (apply-k k (apply vector-set! args))]
      [(display) (apply-k k (apply display args))]
      [(newline) (apply-k k (apply newline args))]
      [(caar) (apply-k k (apply caar args))]
      [(cadr) (apply-k k (apply cadr args))]
      [(cdar) (apply-k k (apply cdar args))]
      [(cddr) (apply-k k (apply cddr args))]
      [(caaar) (apply-k k (apply caaar args))]
      [(caadr) (apply-k k (apply caadr args))]
      [(cadar) (apply-k k (apply cadar args))]
      [(caddr) (apply-k k (apply caddr args))]
      [(cdaar) (apply-k k (apply cdaar args))]
      [(cdadr) (apply-k k (apply cdadr args))]
      [(cddar) (apply-k k (apply cddar args))]
      [(cdddr) (apply-k k (apply cdddr args))]
      [(quotient) (apply-k k (apply quotient args))]
      [(list-tail) (apply-k k (apply list-tail args))]
      [(append) (apply-k k (apply append args))]
      [(eqv?) (apply-k k (apply eqv? args))]
      [(newline) (apply-k k (apply newline args))]
      [(display) (apply-k k (apply display args))]
      [(list) (apply-k k args)]
      ;; assignment 14
      [(apply) (apply-proc (car args) (cadr args) k)]
      [(map)
        (if (null? (cadr args))
          (apply-k k (cadr args))
          (apply-k k (apply-proc (car args) (caadr args) (map-k (car args) (cdadr args)))))]
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










