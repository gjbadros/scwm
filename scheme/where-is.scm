;;; $Id$
;;; describe.scm
;;; Copyright (C) 1999, 2000 Greg J. Badros

(define-module (app scwm where-is)
  :use-module (app scwm optargs)
  :use-module (app scwm message-window)
  :use-module (app scwm gtk-message)
  :use-module (app scwm prompt-proc)
  :use-module (app scwm reflection))

(define (where-is-description proc)
  (if (procedure? proc) 
      (let ((description (procedure->bindings-description proc)))
	(if (> (string-length description) 0)
	    (gtk-message description #:title (string-append "Bindings for " (procedure-name proc)))
	    (display-message-briefly (string-append "No bindings for " (procedure-name proc)))))
      (display-message-briefly (string-append (with-output-to-string (lambda () (write proc)))
					      " is not a procedure"))))

;; (where-is describe-event)
;; (where-is 'describe-event)
;; (where-is "describe-key")
;; (where-is)
(define*-public (where-is #&optional (proc #f))
  "Show bindings that invoke PROC.
Pops up a procedure selection window if PROC is omitted or #f.
PROC can be the procedure object, a symbol, or a string; 
see `interpret-as-procedure'."
  (interactive)
  (if proc
      (where-is-description (interpret-as-procedure proc))
      (prompt-proc "Where is command:" where-is-description
		   #:title "Lookup bindings")))
