;; $Id$
;; (C) 1999 Greg J. Badros

(define-module (app scwm constraints)
  :use-module (app scwm ui-constraints-buttons)
  :use-module (app scwm ui-constraints-toggle-menu)
  :use-module (app scwm ui-constraints)
  :use-module (app scwm modifier-key-bindings))


;; (use-modules (app scwm constraints))
;; (use-modules (app scwm ui-constraints))
;; (use-modules (app scwm ui-constraints-buttons))
;; (use-modules (app scwm ui-constraints-toggle-menu))
;; (use-modules (app scwm modifier-key-bindings))

(define-public (reset-constraints)
  "Reset the constraint solving system.
This switches to a fresh new master solver object, and resets
the global list of ui-constraint instances (so the menu of
constraints is reset to empty)."
  (define-public solver (make-cl-solver))
  (scwm-set-master-solver! solver)
  (set! global-constraint-instance-list '()))

(define ui-constraints-window #f)

(define-public (start-constraints)
  "Start using the constraint solver."
  (reset-constraints)
  (install-constraints-ui-features)
  (set! ui-constraints-window
    (start-ui-constraints-buttons)))

(define-public (end-constraints)
  "Terminate using the constraint solver.  
Can restart with a fresh solver by using `start-constraints'."
  (close-ui-constraints-buttons ui-constraints-window)
  (reset-constraints))

(define (install-constraints-ui-features)
  (bind-three-modifier-key-events 
   XKM_CONTROL_L  XKM_ALT_L  XKM_SHIFT_L
   ;; (37 . 4) (115 . 16) (50 . 1)
   draw-all-constraints
   undraw-all-constraints)
  
  (bind-key 'all "C-M-S-c" popup-ui-constraints-toggle-menu))

;; and now start everything
(start-constraints)
