;; $Id$

;; The dynamic-wind calling with-window macro used to illustrate a bug
;;  when used w/ interactive-move
;; if broadcast-config-hook is non-empty then select-window used to 
;; get called multiple times w/o explanation. 
;; I can't reproduce this any more --03/24/99 gjb

(reset-hook! broadcast-config-hook)

(define (a-proc type win) (write-all #t type ", " win) (display "\n"))

(add-hook! broadcast-config-hook a-proc)

(with-window (select-window-interactively)
	     (interactive-move))
