
(reset-hook! invalid-interaction-hook)

(reset-hook! cannot-grab-hook)

(add-hook! invalid-interaction-hook
	   (lambda () (beep) (display "inv interaction\n")))

(add-hook! cannot-grab-hook
	   (lambda () (beep) (display "cannot grab\n")))


