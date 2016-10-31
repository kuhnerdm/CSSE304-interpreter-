                                        ; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (map box vals) env)))

(define deref
  (lambda (x)
    (let ((result (unbox x)))
      result)))

(define set-ref! set-box!)

(define get-offset-in-global
  (lambda (sym env)
    (list-find-position sym (cadr env))))

(define apply-env-ref
  (lambda (env offset depth succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (dgprint env)
    (dgprint offset)
    (dgprint depth)
    (cases environment env
      (empty-env-record ()
        (succeed (apply-env-ref init-env (get-offset-in-global offset init-env) 0 (lambda (v) v)
                   (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                                "variable not found in environment: ~s" offset)))))
      (extended-env-record (syms vals env)
        (if (equal? -1 depth) ; Global env
            (succeed (apply-env-ref init-env (get-offset-in-global offset init-env) 0 (lambda (v) v)
                       (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                         "variable not found in environment: ~s" offset))))
          (if (equal? 0 depth) ; Current env
            (succeed (list-ref vals offset))
            (apply-env-ref env offset (- depth 1) succeed fail))))
      (recursively-extended-env-record (procnames idss bodiess old-env)
        (if (equal? -1 depth)
          (succeed (apply-env-ref init-env offset depth (lambda (v) v) fail))
          (if (equal? 0 depth)
            (if (list? (list-ref idss offset))
              (succeed (box (closure (list-ref idss offset)
                (list-ref bodiess offset)
                env)))
              (succeed (box (closure-pair (list-ref idss offset)
                (list-ref bodiess offset)
                env))))
            (apply-env-ref old-env offset (- depth 1) succeed fail)))))))

(define apply-env
  (lambda (env offset depth succeed fail)
    (succeed (apply-env-ref env offset depth deref fail))))

(define extend-env-recursively
  (lambda (proc-names idss bodiess old-env)
    (recursively-extended-env-record 
      proc-names idss bodiess old-env)))
