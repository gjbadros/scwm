;; $Id$
;;(window-context)

(define w (get-window))

(list
 (window-context)
 (with-window w
	      (window-context))
 (window-context))
