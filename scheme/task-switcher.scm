;;; $Id$
;;; task-switcher.scm
;;; Copyright (C) 1999, 2000 Greg J. Badros

(define-module (app scwm task-switcher)
  :use-module (app scwm optargs)
  :use-module (app scwm winlist-menu)
  :use-module (app scwm winlist))

(define*-public (window-task-switcher-menu #&optional (last? #f) #&rest rest)
  "Popup a task-switcher window list.
Start out on the window that last had the focus."
  (interactive)
  ;; Skip over the title and first window in the list 
  ;; (that win has focus already)
  (apply show-window-list-menu 
	 (append 
	  (list (if last? -1 3) #t 
		#:by-focus #t #:show-last-focus-time #t #:show-geometry #f)
	  rest)))

;; (window-task-switcher-menu #f #:only (lambda (w) (not (iconified-window? w))))
  
(define*-public (window-task-switcher-menu-backwards . rest)
  "Popup a task-switcher window list.
Start out on the window that had the focus longest ago."
  (interactive)
  (apply window-task-switcher-menu (cons #t rest)))
