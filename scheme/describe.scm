;;; $Id$
;;; describe.scm
;;; (C) 1999 Greg J. Badros

(define-module (app scwm describe)
  :use-module (app scwm optargs)
  :use-module (app scwm message-window)
  :use-module (app scwm reflection))

(define*-public (describe-key)
  "Display a message giving the bindings of the next key event."
  (interactive)
  (let* ((key (car (get-key-event)))
	 (procs (lookup-key 'all key)))
    (display-message-briefly
     (string-append key " is bound to " 
		    (if (pair? procs)
			(string-append (procedure->string (car procs))
				       ", "
				       (procedure->string (cadr procs)))
			"nothing")))))

(define*-public (describe-mouse)
  "Display a message giving the bindings of the next mouse event."
  (interactive)
  (let* ((key (car (get-mouse-event)))
	 (procs (lookup-mouse 'all key)))
    (display-message-briefly
     (string-append key " is bound to " 
		    (if (pair? procs)
			(string-append (procedure->string (car procs))
				       ", "
				       (procedure->string (cadr procs)))
			"nothing")))))
