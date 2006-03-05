;;; $Id$
;;; By Greg J. Badros, --01/18/99 gjb

(use-modules (app scwm string-prompt))
(use-modules (gtk gtk))

(define* (rename-window-interactively #:optional (win (get-window)))
  (string-prompt "Rename to: " (lambda (new-name)
				 (set-window-title! win new-name))
		 "Rename-window"))

(rename-window-interactively)
