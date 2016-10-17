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
  (lambda (datum name exp)
    (if (and (list? (2nd datum)) (for-all (lambda (x) (and (list? x) (eq? (length x) 2))) (2nd datum)))
        (if (for-all (lambda (x) (symbol? (car x))) (2nd datum))
            (if (> (length datum) 2)
                (exp (map car (2nd datum))
                  (map (lambda (x) (parse-exp (2nd x))) (2nd datum))
                  (map parse-exp (cddr datum)))
                (eopl:error 'parse-exp "Bad ~s: wrong length (no bodies or vars): ~s" name datum))
            (eopl:error 'parse-exp "Bad ~s: improper var definition (not a symbol): ~s" name datum))
        (eopl:error 'parse-exp "Bad ~s: improper var definition (not a touple): ~s" name datum))))


(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
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
                 (lambda-exp-sym (2nd datum) (map parse-exp (cddr datum)))]
                [((list-of symbol?) (2nd datum)) ; (lambda (x y z) q ...)
                 (lambda-exp-list (2nd datum) (map parse-exp (cddr datum)))]
                [(and (pair? (2nd datum)) (not (list? (2nd datum)))) ; (lambda (x y . z) q ...)
                 (lambda-exp-improper (2nd datum) (map parse-exp (cddr datum)))]
                [else (eopl:error 'parse-exp "bad lambda: bad types for arguments ~s" datum)])])]
         [(eqv? (car datum) 'let) ;; (let ([pairs]) bodies)
          (if (symbol? (2nd datum))
              ;; named let
              (if (and (list? (3nd datum)) (for-all (lambda (x) (and (list? x) (eq? (length x) 2))) (3nd datum)))
                  (if (for-all (lambda (x) (symbol? (car x))) (3nd datum))
                      (if (> (length datum) 3)
                          (namedlet-exp (2nd datum)
                            (map car (3nd datum))
                            (map (lambda (x) (parse-exp (2nd x))) (3nd datum))
                            (map parse-exp (cdddr datum)))
                          (eopl:error 'parse-exp "Bad named let: wrong length (no bodies or vars): ~s" datum))
                      (eopl:error 'parse-exp "Bad named let: improper var definition (not a symbol): ~s" datum))
                  (eopl:error 'parse-exp "Bad named let: improper var definition (not a touple): ~s" datum))
              ;; unnamed let
              (toLet-exp datum 'let let-exp))]
         ;; other lets
         [(eqv? (car datum) 'let*)
          (toLet-exp datum 'let* let*-exp)]
         [(eqv? (car datum) 'letrec)
          (toLet-exp datum 'letrec letrec-exp)]
         ;; (set! var exp)
         [(eqv? (car datum) 'set!)
          (if (eq? (length datum) 3)
              (if (symbol? (2nd datum))
                  (set!-exp (2nd datum) (parse-exp (3rd datum)))
                  (eopl:error 'parse-exp "Bad set!: not setting var: ~s" datum))
              (eopl:error 'parse-exp "Bad set!: too many or too few arguments: ~s" datum))]
         ;; (if pred true false)
         [(eqv? (car datum) 'if)
          (cond
            [(eq? (length datum) 3)
             (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) '())]
            [(eq? (length datum) 4)
             (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))]
            [else (eopl:error 'parse-exp "Bad if: too many or two few argumetns: ~s" datum)])]              
         [(list? datum)
          (app-exp (parse-exp (1st datum))
            (map parse-exp (cdr datum)))]
         [else
           (eopl:error 'parse-exp "Bad expression, not a proper list: ~s" datum)])]
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define (unparse-exp datum)
  (cases expression datum
    [lit-exp (val) (list 'quote val)]
    [var-exp (id) id]
    [lambda-exp-list (vars body)
      (cons 'lambda (cons vars (map unparse-exp body)))]
    [lambda-exp-sym (vars body)
      (cons 'lambda (cons vars (map unparse-exp body)))]
    [lambda-exp-improper (vars body)
      (cons 'lambda (cons vars (map unparse-exp body)))]
    [app-exp (rator rands)
      (cons (unparse-exp rator) (map unparse-exp rands))]
    [let-exp (var exp body)
      (append (list 'let (map list var (map unparse-exp exp))) (map unparse-exp body))]
    [let*-exp (var exp body)
      (append (list 'let* (map list var (map unparse-exp exp))) (map unparse-exp body))]
    [letrec-exp (var exp body)
      (append (list 'letrec (map list var (map unparse-exp exp))) (map unparse-exp body))]
    [namedlet-exp (name var exp body)
      (append (list 'let name (map list var (map unparse-exp exp))) (map unparse-exp body))]
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
      [letrec-exp (var exp body)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [namedlet-exp (name var exp body)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [set!-exp (var val)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)]
      [if-exp (con thn els)
        (eopl:error 'syntax-expand "Bad expansion, get-rator-rands detected ~s" datum)])))

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
           (app-exp (lambda-exp-list '() (map syntax-expand rands)) '())]
          [(equal? rator (var-exp 'and))
           (if (null? (cdr rands))
               (app-exp (lambda-exp-list '(intpTempVal) (list (if-exp (var-exp 'intpTempVal) (var-exp 'intpTempVal) (lit-exp #f)))) (list (syntax-expand (car rands))))
               (if-exp (car rands) (syntax-expand (app-exp (var-exp 'and) (cdr rands))) (lit-exp #f)))]
          [(equal? rator (var-exp 'or))
           (if (null? (cdr rands))
               (app-exp (lambda-exp-list '(intpTempVal) (list (if-exp (var-exp 'intpTempVal) (var-exp 'intpTempVal) (lit-exp #f)))) (list (syntax-expand (car rands))))
               (app-exp (lambda-exp-list '(intpTempVal) (list (if-exp (var-exp 'intpTempVal) (var-exp 'intpTempVal) (syntax-expand (app-exp (var-exp 'or) (cdr rands)))))) (list (car rands))))]
          [else (app-exp rator (map syntax-expand rands))])]
      [lit-exp (val) datum]
      [var-exp (id) datum]
      [lambda-exp-list (vars body)
        (lambda-exp-list vars (map syntax-expand body))]
      [lambda-exp-sym (vars body)
        (lambda-exp-sym vars (map syntax-expand body))]
      [lambda-exp-improper (vars body)
        (lambda-exp-improper vars (map syntax-expand body))]
      [let-exp (var exp body)
        (app-exp (lambda-exp-list var (map syntax-expand body)) (map syntax-expand exp))]
      [let*-exp (var exp body)
        (cond 
          [(null? var) (app-exp (lambda-exp-list var (map syntax-expand body)) (map syntax-expand exp))]
          [(null? (cdr var)) (app-exp (lambda-exp-list var (map syntax-expand body)) (map syntax-expand exp))]
          [else (syntax-expand
                  (app-exp (lambda-exp-list (car var) (syntax-expand (let*-exp (cdr var) (cdr exp) body))) (syntax-expand (car exp))))])]
      [letrec-exp (var exp body)
        (letrec-exp var (map syntax-expand exp) (map syntax-expand body))]
      [namedlet-exp (name var exp body)
        (namedlet-exp var (map syntax-expand exp) (map syntax-expand body))]
      [set!-exp (var val)
        (set!-exp var (syntax-expand val))]
      [if-exp (con thn els)
        (if-exp (syntax-expand con) (syntax-expand thn) (if (null? els) els (syntax-expand els)))])))
