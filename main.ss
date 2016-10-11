; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014

(load "c:/users/kuhnerdm/desktop/git_kuhnerdm/csse304-interpreter-/chez-init.ss")

(define load-all ; make it easy to reload the files
  (lambda ()
  	(load "c:/users/kuhnerdm/desktop/git_kuhnerdm/csse304-interpreter-/helpers.ss")
    (load "c:/users/kuhnerdm/desktop/git_kuhnerdm/csse304-interpreter-/datatypes.ss")
    (load "c:/users/kuhnerdm/desktop/git_kuhnerdm/csse304-interpreter-/parse.ss")
    (load "c:/users/kuhnerdm/desktop/git_kuhnerdm/csse304-interpreter-/env.ss")
    (load "c:/users/kuhnerdm/desktop/git_kuhnerdm/csse304-interpreter-/interpreter.ss")))

(load-all)

(define l load-all) ; even easier!
