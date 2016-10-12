;; Procedures that we are overwriting or creating

;; this is will return false if val is not in list ls
;; otherwise it returns the val
(define (contains val ls)
  (find (lambda (x) (eqv? x val)) ls))

;; map that will always run in order from left to right
(define (map proc ls)
  (reverse (fold-left (lambda (prev next) (cons (proc next) prev)) '() ls)))

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
(define (debug this)
  (display this)
  (newline))
