;;; $Id$ -*- scwm -*-

;; this will segfault
(bind-mouse 'all "H-3" (lambda (f) (interactive-resize (get-window) #f)))

;; Just for test bind, unbind-mouse
(unbind-mouse 'all "C-S-Mouse1")
(unbind-mouse 'all "C-S-M-2")
(bind-mouse 'all "C-S-Mouse-1" show-icon-list-menu)

(bind-mouse 'all "C-S-M-3" (lambda () (interactive-resize (get-window) #t)))
(bind-mouse 'all "H-3" (lambda () (interactive-resize (get-window) #f)))
(recapture)

(select-window-interactively)
