(define or2Type
  (lambda (t1 t2)
    (lambda (x)
      (or (t1 x) (t2 x)))))

(define is-quoted-list?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'quote) (list? (cadr x)) (null? (cddr x)))))

(define is-quoted-vector?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'quote) (vector? (cadr x)) (null? (cddr x)))))

(define is-quoted-symbol?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'quote) (symbol? (cadr x)) (null? (cddr x)))))

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define toLet-exp
  (lambda (datum definedVars name exp)
    (if (and (list? (2nd datum)) (for-all (lambda (x) (and (list? x) (eq? (length x) 2))) (2nd datum)))
        (if (for-all (lambda (x) (symbol? (car x))) (2nd datum))
            (if (> (length datum) 2)
                (let ([decVars (mapIO car (2nd datum))]) 
                  (exp decVars
                    (mapIO (lambda (x) (parse-exp (2nd x) definedVars)) (2nd datum))
                    (mapIO (parse-exp-mapIO (cons decVars definedVars)) (cddr datum))))
                (eopl:error 'parse-exp "Bad ~s: wrong length (no bodies or vars): ~s" name datum))
            (eopl:error 'parse-exp "Bad ~s: improper var definition (not a symbol): ~s" name datum))
        (eopl:error 'parse-exp "Bad ~s: improper var definition (not a touple): ~s" name datum))))

(define get-lexical
  (lambda (sym curVars)
    (let loop ([curVars curVars] [layer 0])
      (if (null? curVars)
          (cons -1 sym) ;; check in globals, then fail with elop error or something
          (let ([pos (list-find-position sym (car curVars))])
            (if (number? pos)
                (cons pos layer)
                (loop (cdr curVars) (+ 1 layer))))))))

(define (parse-exp-mapIO definedVars)
  (lambda (exp)
    (parse-exp exp definedVars)))

(define parse-exp         
  (lambda (datum definedVars)
    (cond
      [(symbol? datum)
       (let ([lexAddr (get-lexical datum definedVars)])
         (var-exp (car lexAddr) (cdr lexAddr)))]
      [(number? datum) (lit-exp datum)]
      [(string? datum) (lit-exp datum)]
      [(is-quoted-list? datum) (lit-exp (cadr datum))]
      [(is-quoted-vector? datum) (lit-exp (cadr datum))]
      [(is-quoted-symbol? datum) (lit-exp (cadr datum))]
      [(boolean? datum) (lit-exp datum)]
      [(vector? datum) (lit-exp datum)]
      [(pair? datum)
       (cond
         [(eqv? (car datum) 'quote) ;; (quote this)
          (lit-exp `',datum)]
         [(eqv? (car datum) 'lambda)
          (cond
            [(null? (cdr datum)) ; (lambda)
             (eopl:error 'parse-exp "bad lambda: no body, no variables ~s" datum)]
            [(null? (cddr datum)) ; (lambda x)
             (eopl:error 'parse-exp "bad lambda: no body ~s" datum)]
            [else ; (lambda [something] y ...)
              (cond
                [(symbol? (2nd datum)) ; (lambda x y ...)
                 (lambda-exp-sym (2nd datum)
                   (mapIO (parse-exp-mapIO (cons (list (2nd datum)) definedVars)) (cddr datum)))]
                [((list-of symbol?) (2nd datum)) ; (lambda (x y z) q ...)
                 (lambda-exp-list (2nd datum)
                   (mapIO (parse-exp-mapIO (cons (2nd datum) definedVars)) (cddr datum)))]
                [(and (pair? (2nd datum)) (not (list? (2nd datum)))) ; (lambda (x y . z) q ...)
                 (lambda-exp-improper (2nd datum)
                   (mapIO (parse-exp-mapIO (cons (flatten (2nd datum)) definedVars)) (cddr datum)))]
                [else (eopl:error 'parse-exp "bad lambda: bad types for arguments ~s" datum)])])]
         [(eqv? (car datum) 'let) ;; (let ([pairs]) bodies)
          (if (symbol? (2nd datum))
              ;; named let
              (if (and (list? (3rd datum)) (for-all (lambda (x) (and (list? x) (eq? (length x) 2))) (3rd datum)))
                  (if (for-all (lambda (x) (symbol? (car x))) (3rd datum))
                      (if (> (length datum) 3)
                          (let ([decvars (mapIO car (3rd datum))])
                            (namedlet-exp (2nd datum)
                              decvars
                              (mapIO (lambda (x) (parse-exp (2nd x) definedVars)) (3rd datum))
                              (mapIO
                                (parse-exp-mapIO
                                  (cons (list (2nd datum)) (cons decvars definedVars)))
                                (cdddr datum))))
                          (eopl:error 'parse-exp "Bad named let: wrong length (no bodies or vars): ~s" datum))
                      (eopl:error 'parse-exp "Bad named let: improper var definition (not a symbol): ~s" datum))
                  (eopl:error 'parse-exp "Bad named let: improper var definition (not a touple): ~s" datum))
              ;; unnamed let
              (toLet-exp datum definedVars 'let let-exp))]
         ;; other lets
         [(eqv? (car datum) 'let*)
          (toLet-exp datum definedVars 'let* let*-exp)]
         [(eqv? (car datum) 'letrec)
          (if (and (list? (2nd datum)) (for-all (lambda (x) (and (list? x) (eq? (length x) 2))) (2nd datum)))
              (if (for-all (lambda (x) (symbol? (car x))) (2nd datum))
                  (if (> (length datum) 2)
                      (let ([proc-names (mapIO car (cadr datum))] [idss (mapIO cadadr (cadr datum))])
                        (letrec-exp
                          proc-names ; Proc names
                          idss ; idss
                          (map (lambda (proc-name ids x)
                                 (mapIO
                                   (parse-exp-mapIO (cons ids (cons proc-name definedVars)))
                                   (cddadr x)))
                            proc-names idss (cadr datum)) ; bodiess
                          (mapIO (parse-exp-mapIO (cons proc-names definedVars)) (cddr datum)))) ; letrec-bodies
                      (eopl:error 'parse-exp "Bad ~s: wrong length (no bodies or vars): ~s" 'letrec datum))
                  (eopl:error 'parse-exp "Bad ~s: improper var definition (not a symbol): ~s" 'letrec datum))
              (eopl:error 'parse-exp "Bad ~s: improper var definition (not a touple): ~s" 'letrec datum))]
         ;; (set! var exp)
         [(eqv? (car datum) 'set!)
          (if (eq? (length datum) 3)
              (if (symbol? (2nd datum))
                  (set!-exp (get-lexical (2nd datum) definedVars) (parse-exp (3rd datum) definedVars))
                  (eopl:error 'parse-exp "Bad set!: not setting var: ~s" datum))
              (eopl:error 'parse-exp "Bad set!: too many or too few arguments: ~s" datum))]
         ;; (if pred true false)
         [(eqv? (car datum) 'if)
          (cond
            [(eq? (length datum) 3)
             (if-exp (parse-exp (2nd datum) definedVars)
               (parse-exp (3rd datum) definedVars)
               '())]
            [(eq? (length datum) 4)
             (if-exp (parse-exp (2nd datum) definedVars)
               (parse-exp (3rd datum) definedVars)
               (parse-exp (4th datum) definedVars))]
            [else (eopl:error 'parse-exp "Bad if: too many or two few argumetns: ~s" datum)])]
         [(eqv? (car datum) 'define)
          (define-exp (2nd datum) (parse-exp (3rd datum) definedVars))]
         [(list? datum)
          (app-exp (parse-exp (1st datum) definedVars)
            (mapIO (parse-exp-mapIO definedVars) (cdr datum)))]
         [else
           (eopl:error 'parse-exp "Bad expression, not a proper list: ~s" datum)])]
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define (unparse-exp datum)
  (cases expression datum
    [lit-exp (val) (list 'quote val)]
    [var-exp (id) id]
    [lambda-exp-list (vars body)
      (cons 'lambda (cons vars (mapIO unparse-exp body)))]
    [lambda-exp-sym (vars body)
      (cons 'lambda (cons vars (mapIO unparse-exp body)))]
    [lambda-exp-improper (vars body)
      (cons 'lambda (cons vars (mapIO unparse-exp body)))]
    [app-exp (rator rands)
      (cons (unparse-exp rator) (mapIO unparse-exp rands))]
    [let-exp (var exp body)
      (append (list 'let (mapIO list var (mapIO unparse-exp exp))) (mapIO unparse-exp body))]
    [let*-exp (var exp body)
      (append (list 'let* (mapIO list var (mapIO unparse-exp exp))) (mapIO unparse-exp body))]
    [letrec-exp (var exp body)
      (append (list 'letrec (mapIO list var (mapIO unparse-exp exp))) (mapIO unparse-exp body))]
    [namedlet-exp (name var exp body)
      (append (list 'let name (mapIO list var (mapIO unparse-exp exp))) (mapIO unparse-exp body))]
    [set!-exp (var val)
      (append (list 'set! var (unparse-exp val)))]
    [if-exp (con thn els)
      (append (list 'if (unparse-exp con) (unparse-exp thn)) (if (null? els) '() (list (unparse-exp els))))]))

(define get-rator-rands
  (lambda (datum)
    (cases expression datum
      [lit-exp (val)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [var-exp (id)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [lambda-exp-list (vars body)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [lambda-exp-sym (vars body)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [lambda-exp-improper (vars body)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [app-exp (rator rands)
        (cons rator rands)]
      [let-exp (var exp body)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [let*-exp (var exp body)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [letrec-exp (var idss exp body)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [namedlet-exp (name var exp body)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [set!-exp (var val)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [if-exp (con thn els)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [else (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)])))

(define syntax-expand
  (lambda (datum)
    (cases expression datum
      [app-exp (rator rands)
        (cond
          [(equal? rator (var-exp 'cond))
           (cond
             [(equal? (car (get-rator-rands (car rands))) (var-exp 'else)) (syntax-expand (cadr (get-rator-rands (car rands))))]
             [(null? (cdr rands))
              (if-exp (syntax-expand (car (get-rator-rands (car rands)))) (syntax-expand (cadr (get-rator-rands (car rands)))) '())]
             [else
               (if-exp (syntax-expand (car (get-rator-rands (car rands))))
                 (syntax-expand (cadr (get-rator-rands (car rands))))
                 (syntax-expand (app-exp (var-exp 'cond) (cdr rands))))])]
          [(equal? rator (var-exp 'begin))
           (app-exp (lambda-exp-list '() (mapIO syntax-expand rands)) '())]
          [(equal? rator (var-exp 'and))
           (if (null? (cdr rands))
               (app-exp (lambda-exp-list '(intpTempVal) (list (if-exp (var-exp 'intpTempVal) (var-exp 'intpTempVal) (lit-exp #f)))) (list (syntax-expand (car rands))))
               (if-exp (car rands) (syntax-expand (app-exp (var-exp 'and) (cdr rands))) (lit-exp #f)))]
          [(equal? rator (var-exp 'or))
           (if (null? rands) (lit-exp #f)
               (if (null? (cdr rands))
                   (app-exp (lambda-exp-list '(intpTempVal) (list (if-exp (var-exp 'intpTempVal) (var-exp 'intpTempVal) (lit-exp #f)))) (list (syntax-expand (car rands))))
                   (app-exp (lambda-exp-list '(intpTempVal) (list (if-exp (var-exp 'intpTempVal) (var-exp 'intpTempVal) (syntax-expand (app-exp (var-exp 'or) (cdr rands)))))) (list (syntax-expand (car rands))))))]
          [(equal? rator (var-exp 'while))
           (letrec-exp '(mainWhileLoop)
             (list (lambda-exp-list '() (list (if-exp (syntax-expand (car rands)) (app-exp (lambda-exp-list '() (mapIO syntax-expand (append (cdr rands) (list (app-exp (var-exp 'mainWhileLoop) '()))))) '()) '()))))
             (list (app-exp (var-exp 'mainWhileLoop) '())))]
          [else (app-exp rator (mapIO syntax-expand rands))])]
      [lit-exp (val) datum]
      [var-exp (id) datum]
      [lambda-exp-list (vars body)
        (lambda-exp-list vars (mapIO syntax-expand body))]
      [lambda-exp-sym (vars body)
        (lambda-exp-sym vars (mapIO syntax-expand body))]
      [lambda-exp-improper (vars body)
        (lambda-exp-improper vars (mapIO syntax-expand body))]
      [let-exp (var exp body)
        (app-exp (lambda-exp-list var (mapIO syntax-expand body)) (mapIO syntax-expand exp))]
      [let*-exp (var exp body)
        (cond 
          [(null? var) (app-exp (lambda-exp-list var (mapIO syntax-expand body)) (mapIO syntax-expand exp))]
          [(null? (cdr var)) (app-exp (lambda-exp-list var (mapIO syntax-expand body)) (mapIO syntax-expand exp))]
          [else (syntax-expand
                  (app-exp (lambda-exp-list (list (car var)) (list (syntax-expand (let*-exp (cdr var) (cdr exp) body)))) (list (syntax-expand (car exp)))))])]
      [letrec-exp (proc-names idss bodiess letrec-bodies)
        (letrec-exp proc-names idss (mapIO (lambda (x) (mapIO syntax-expand x)) bodiess) (mapIO syntax-expand letrec-bodies))]
      [namedlet-exp (name var exp body)
        (syntax-expand (letrec-exp (list name) (list var) (list body) (list (app-exp (var-exp name) exp))))]
      [set!-exp (var val)
        (set!-exp var (syntax-expand val))]
      [if-exp (con thn els)
        (if-exp (syntax-expand con) (syntax-expand thn) (if (null? els) els (syntax-expand els)))]
      [define-exp (var val)
        (define-exp var (syntax-expand val))])))
