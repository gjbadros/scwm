;; $Id$
;; (C) 1999 Greg J. Badros

(define-module (app scwm constraints)
  :use-module (app scwm ui-constraints-buttons)
  :use-module (app scwm ui-constraints-toggle-menu)
  :use-module (app scwm ui-constraints)
  :use-module (app scwm modifier-key-bindings))


;; (use-modules (app scwm constraints))
;; (use-modules (app scwm ui-constraints-buttons))
;; (use-modules (app scwm ui-constraints-toggle-menu))

(start-ui-constraints-buttons) 

(bind-three-modifier-key-events 
 XKM_CONTROL_L  XKM_ALT_L  XKM_SHIFT_L
;; (37 . 4) (115 . 16) (50 . 1)
 draw-all-constraints
 undraw-all-constraints)

(bind-key 'all "C-M-S-c" popup-ui-constraints-toggle-menu)
