;;; $Id$
;;; Copyright (C) 1999, 2000 Greg J. Badros
;;; See also the `window-visibility' primitive

(define-module (app scwm visibility-reporter))

(define (report-unobscured win from-viewport-move?)
  (display "unobscured") (write win)
  (display " from viewport move ") (display from-viewport-move?)
  (newline))
(define (report-partially-obscured win from-viewport-move?)
  (display "partially-obscured") (write win)
  (display " from viewport move ") (display from-viewport-move?)
  (newline))
(define (report-fully-obscured win from-viewport-move?)
  (display "fully-obscured") (write win)
  (display " from viewport move ") (display from-viewport-move?)
  (newline))

(define-public (install-visibility-reporter)
  "Install procedures to print debugging messages on window visibility change events."
  (add-hook! window-unobscured-hook report-unobscured)
  (add-hook! window-fully-obscured-hook report-fully-obscured)
  (add-hook! window-partially-obscured-hook report-partially-obscured))

(define-public (uninstall-visibility-reporter)
  "Uninstall procedures that print debugging messages on window visibility change events."
  (remove-hook! window-unobscured-hook report-unobscured)
  (remove-hook! window-fully-obscured-hook report-fully-obscured)
  (remove-hook! window-partially-obscured-hook report-partially-obscured))
