;; $Id$

(xtest-supported?)

(bind-key 'all "H-y" (lambda ()
		       (xtest-fake-key-event 37 #t 10)
		       (xtest-fake-key-event 38 #t 11)
		       (xtest-fake-key-event 38 #f 12)
		       (xtest-fake-key-event 37 #f 13)))


(bind-key 'all "H-y" (lambda () 
		       (xtest-fake-motion-event 10 10 0)))


(for-each (lambda (keysym press?) (xtest-fake-key-event (car (keysym->keycode keysym)) press?))
	  '("Control_L" "Meta_L" "Shift_L" "z" "z" "Shift_L" "Meta_L" "Control_L")
	  '(#t #t #t #t #f #f #f #f))
