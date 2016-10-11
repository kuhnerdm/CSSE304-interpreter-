;; Procedures that we are overwriting or creating

;; this is will return false if val is not in list ls
;; otherwise it returns the val
(define (contains val ls)
  (find (lambda (x) (eqv? x val)) ls))

;; map that will always run in order from left to right
(define (map proc ls)
  (fold-right (lambda (next prev) (cons (proc next) prev)) '() ls))

