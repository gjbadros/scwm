;;; $Id$
;;; By Greg J. Badros, --01/18/99 gjb

(use-modules (app scwm string-prompt))

(let ((w (select-window-interactively "choose window to rename")))
  (string-prompt "Rename to: " (lambda (new-name)
				 (set-window-title! w new-name))))
