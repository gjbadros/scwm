;;;; $Id$
;;;; Copyright (C) 2000 Greg J. Badros <gjb@cs.washington.edu>
;;;; This does not work because:
;;;; prompt-string.scm:49:33: In procedure continuation in expression (proc (getter)):
;;;; prompt-string.scm:49:33: continuation from wrong top level


(define-module (app scwm string-answer)
  :use-module (gtk gtk)
  :use-module (app scwm prompt-string))

(define call/cc call-with-current-continuation)

(define (string-answer prompt)
  (call-with-current-continuation
   (lambda (rest)
     (prompt-string prompt rest))))

;;; (begin (display (string-answer "Foo?")) (newline))
