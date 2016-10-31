;; Procedures that we are overwriting or creating

;; this is will return false if val is not in list ls
;; otherwise it returns the val
(define (contains val ls)
  (find (lambda (x) (eqv? x val)) ls))

;; map that will always run in order from left to right
(define (mapIO proc ls)
  (reverse (fold-left (lambda (prev next) (cons (proc next) prev)) '() ls)))

;; for the lambda with an improper list
(define (pairs-of proc)
  (lambda (l)
    (let loop ([l l])
      (cond [(or (proc l) (null? l)) #t]
            [(and (pair? l) (proc (car l))) (loop (cdr l))]
            [else #f]))))

(define (pair-of proc)
  (lambda (l)
    (and (pair? l) (proc (car l)) (proc (cdr l)))))

;; things that can be a literal
(define is-literal?
  (lambda (x)
    (or
      (number? x)
      (string? x)
      (list? x)
      (symbol? x)
      (boolean? x)
      (vector? x))))

;; makes inline debugging easier
(define (dgprint this)
  (display this)
  (newline))

(define list-ref
  (lambda (list pos)
    (let loop ([ls list] [p pos])
      (cond
        [(eq? p 0) (if (pair? ls) (car ls) ls)]
        [(pair? list) (loop (cdr ls) (- p 1))]
        [else (eopl:error 'list-ref "list-ref failed at ~s ~s" list pos)]))))

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
