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

(define (flatten implst)
  (if (list? implst)
      implst
      (let loop ([implst implst])
        (if (pair? implst)
            (cons (car implst) (loop (cdr implst)))
            (cons implst '())))))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) (flatten los))))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ 1 list-index-r)
                  #f))))))

(define apply-env-ref
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
              (succeed (list-ref vals pos))
              (apply-env-ref env sym succeed fail))))
      (recursively-extended-env-record (procnames idss bodiess old-env)
        (let ([pos (list-find-position sym procnames)])
          (if (number? pos)
              (if (list? (list-ref idss pos)) 
                  (closure (list-ref idss pos)
                    (list-ref bodiess pos)
                    env)
                  (closure-pair (list-ref idss pos)
                    (list-ref bodiess pos)
                    env))
              (apply-env old-env sym succeed fail)))))))

(define apply-env
  (lambda (env sym succeed fail)
    (succeed (apply-env-ref env sym (lambda (v) (deref v)) fail))))

(define extend-env-recursively
  (lambda (proc-names idss bodiess old-env)
    (recursively-extended-env-record 
      proc-names idss bodiess old-env)))
