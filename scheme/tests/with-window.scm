;; $Id$

;; The dynamic-wind calling with-window macro illustrates a bug
;;  when used w/ interactive-move
;; if broadcast-config-hook is non-empty whereby select-window
;; gets called multiple times w/o explanation. 

(reset-hook! broadcast-config-hook)

(add-hook! broadcast-config-hook (lambda (type win) (write-all #t type ", " win) (display "\n")))

(with-window (select-window-interactively)
	     (interactive-move))
