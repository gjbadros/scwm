;; $Id$
;; Copyright (C) 1999 Greg J. Badros

(define-module (app scwm constraints)
  :use-module (app scwm optargs)
  :use-module (app scwm ui-constraints-buttons)
  :use-module (app scwm ui-constraints-toggle-menu)
  :use-module (app scwm ui-constraints)
  :use-module (app scwm modifier-key-bindings))


;; (use-modules (app scwm constraints))
;; (use-modules (app scwm ui-constraints))
;; (use-modules (app scwm ui-constraints-buttons))
;; (use-modules (app scwm ui-constraints-toggle-menu))
;; (use-modules (app scwm modifier-key-bindings))

(define-public solver #f)

(define-public (reset-constraints)
  "Reset the constraint solving system.
This switches to a fresh new master solver object, and resets
the global list of ui-constraint instances (so the menu of
constraints is reset to empty)."
  (set! solver (make-cl-solver))
  (scwm-set-master-solver! solver)
  (set! global-constraint-instance-list '()))

(define ui-constraints-window #f)

(define*-public (start-constraints #&key (draw-constraints-with-focus #f))
  "Start using the constraint solver.
DRAW-CONSTRAINTS-WITH-FOCUS is used to specify whether
you'd like all constraints drawn or just the ones which
are associated with the current focus window."
  (reset-constraints)
  (install-constraints-ui-features draw-constraints-with-focus)
  (set! ui-constraints-window
    (start-ui-constraints-buttons)))

(define-public (end-constraints)
  "Terminate using the constraint solver.  
Can restart with a fresh solver by using `start-constraints'."
  (close-ui-constraints-buttons ui-constraints-window)
  (reset-constraints))

(define (install-constraints-ui-features draw-with-focus)
  (and XKM_CONTROL_L XKM_ALT_L XKM_SHIFT_L XKM_HYPER_L
       (bind-four-modifier-key-events 
	XKM_CONTROL_L  XKM_ALT_L  XKM_SHIFT_L XKM_HYPER_L
	;; (37 . 4) (115 . 16) (50 . 1)
	(if draw-with-focus draw-constraints-with-focus draw-all-constraints)
	(if draw-with-focus undraw-constraints-with-focus undraw-all-constraints)))
  (bind-key 'all "C-M-S-c" popup-ui-constraints-toggle-menu))


;; drawing constraints on only the current-window-with-focus
;; (need to take into account focus changes)

;; hooks

(define (draw-focus-change-hook win)
  (if win (draw-constraints-of-window win)))

(define (draw-focus-lost-hook win)
  (undraw-constraints-of-window win))

(define (draw-constraints-with-focus)
  (let ((win (current-window-with-focus)))
    (if win (draw-constraints-of-window win))
    (add-hook! window-focus-change-hook draw-focus-change-hook)
    (add-hook! window-focus-lost-hook   draw-focus-lost-hook)))

(define (undraw-constraints-with-focus)
  (let ((win (current-window-with-focus)))
    (if win (undraw-constraints-of-window win))
    (remove-hook! window-focus-change-hook draw-focus-change-hook)
    (remove-hook! window-focus-lost-hook   draw-focus-lost-hook)))

