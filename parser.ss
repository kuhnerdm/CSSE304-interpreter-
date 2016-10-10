(load "c:/Users/hirschag/Downloads/work/CSSE304/A011/chez-init.ss")

(define or2Type
  (lambda (t1 t2)
    (lambda (x)
      (or (t1 x) (t2 x)))))

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
  [lambda-exp
    (vars (or2Type symbol? (list-of symbol?)))
    (body (list-of expression?))]
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
      [(or (number? datum)
           (boolean? datum)
           (string? datum)
           (vector? datum)) (lit-exp datum)]
      [(pair? datum)
       (cond
         [(eqv? (car datum) 'quote) ;; (quote this)
          (lit-exp `',datum)]
         [(eqv? (car datum) 'lambda) ;; (lambda (vars) bodies)
          (if (or (symbol? (2nd datum)) (for-all symbol? (2nd datum)) (null? (2nd datum)))
              (if (> (length datum) 2)
                  (lambda-exp (2nd datum) (map parse-exp (cddr datum)))
                  (eopl:error 'parse-exp "Bad lambda: wrong length: ~s" datum))
              (eopl:error 'parse-exp "Bad lambda: check vars: ~s" datum))]
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
    [lit-exp (val) val]
    [var-exp (id) id]
    [lambda-exp (vars body)
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
 
