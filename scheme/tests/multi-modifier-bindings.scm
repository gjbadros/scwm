;; $Id$

(bind-two-modifier-key-events XKM_CONTROL_L XKM_HYPER_L (lambda () (display "now\n")) 
			      (lambda () (display "back\n")))

(unbind-two-modifier-key-events XKM_CONTROL_L XKM_HYPER_L)


(bind-three-modifier-key-events XKM_CONTROL_L XKM_HYPER_L XKM_ALT_L (lambda () (display "now\n")) 
			      (lambda () (display "back\n")))

(unbind-three-modifier-key-events XKM_CONTROL_L XKM_HYPER_L XKM_ALT_L)


(bind-four-modifier-key-events XKM_CONTROL_L XKM_SHIFT_L 
			       XKM_HYPER_L XKM_ALT_L (lambda () (display "now\n")) 
			       (lambda () (display "back\n")))

(unbind-four-modifier-key-events XKM_CONTROL_L XKM_SHIFT_L XKM_HYPER_L XKM_ALT_L)

