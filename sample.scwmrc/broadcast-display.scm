;; $Id$
;; Test broadcast message hooks

(use-modules (app scwm module-types))

(define broadcast-hook
  (lambda (event-type a1 a2 a3 a4 a5 a6 a7)
    (display "broadcast: ")
    (write (list (module-event-name-from-number event-type) 
		   a1 a2 a3 a4 a5 a6 a7))
    (display "\n")
    (if (= event-type M_FOCUS_CHANGE)
	(let ((window (window-from-window-id a1)))
	  (if window (raise-window window))))))

(define broadcast-config-hook 
  (lambda (event-type window)
    (display "broadcast-config: ")
    (write (list (module-event-name-from-number event-type)
		 window))
    (display "\n")))


(define broadcast-name-hook 
  (lambda (event-type a1 a2 a3 name)
    (display "broadcast-name:")
    (write (list (module-event-name-from-number event-type)
		 a1 a2 name))
    (display "\n")))
