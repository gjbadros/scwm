;;; $Id$
(use-scwm-modules session)
(for-each (lambda (hook) 
	    (catch #t
		   (lambda ()
		     (add-hook! (eval hook)
				(lambda args (display (symbol->string hook)) (newline))))
		   (lambda (key . args) #f)))
	  (apropos-internal "-hook$"))


(hook? (eval 'window-leave-hook))

(hook? window-leave-hook)
