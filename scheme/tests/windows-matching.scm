;; $Id$

(use-modules (app scwm gtk)
	     (gtk gtk)
	     (app scwm string-prompt)
	     (app scwm winlist))

(define (popup-window-list-matching-string str)
  "Popup a window list of all windows with titles that match string STR.
Matching is done with wildcards including \"*\" and \"?\"."
  (show-window-list-menu #:only (title-match?? str) #:warp-to-first #t))

(define-public (prompt-winlist-by-name)
  (string-prompt "Windows matching? " popup-window-list-matching-string #:warp #t))

;; (bind-key 'all "C-S-M-w" (thunk prompt-winlist-by-name))


