;;; $Id$
;;; describe.scm
;;; (C) 1999 Greg J. Badros

(define-module (app scwm describe)
  :use-module (app scwm optargs)
  :use-module (app scwm message-window)
  :use-module (app scwm reflection))

(define-public (is-mouse-event? event)
  "Return #t iff EVENT is a mouse event.
EVENT should be a list returned by `get-next-event',
`get-key-event', or `get-mouse-event'."
  (and event (eqv? (length event) 4)))

(define (display-binding key procs)
  (display-message-briefly
   (string-append key " is bound to " 
		  (if (pair? procs)
		      (string-append (procedure->string (car procs))
				     ", "
				     (procedure->string (cadr procs)))
		      "nothing"))))


;;; GJB:FIXME:: handle bindings that are not for the 'all context
(define*-public (describe-event)
  "Display a message giving the bindings of the next event, key or mouse."
  (interactive)
  (let* ((event (get-next-event))
	 (key (car event))
	 (procs ((if (is-mouse-event? event) lookup-mouse lookup-key) 'all key)))
    (display-binding key procs)))

(define*-public (describe-key)
  "Display a message giving the bindings of the next key event."
  (interactive)
  (let* ((key (car (get-key-event)))
	 (procs (lookup-key 'all key)))
    (display-binding key procs)))

(define*-public (describe-mouse)
  "Display a message giving the bindings of the next mouse event."
  (interactive)
  (let* ((key (car (get-mouse-event)))
	 (procs (lookup-mouse 'all key)))
    (display-binding key procs)))
