;; $Id$

(use-modules (app scwm defoption)
	     (app scwm primopts)
	     (app scwm preferences)
	     (app scwm themes)
	     (app scwm menus-extras)
	     (gtk gtk)
	     (gtk gdk))
gdk-lead-window
gdk-leader-window

(use-modules (app scwm gnome-hints))
(popup-menu (scwm-options-menu) #t)

(scwm-options-dialog)

scwm-options

(scwm-option-get *edge-y-scroll*)
(scwm-option-get *desk-width*)

(scwm-option-set! *desk-width* 2)
(scwm-option-set! *desk-height* 2)

(popup-option-menu '*desk-width*)
(popup-option-menu '*desk-height*)
(popup-option-menu '*auto-raise*)

(gui-set '*auto-raise*)
(gui-set '*theme-path*)
(gui-set '*remote-shell-command*)

;; *auto-raise*
;; *edge-y-scroll*
;; *theme-path*
;; (scwm-option-range '*gnome-desktop-number*)
;; (gui-set '*gnome-desktop-number*)

(begin
  (use-modules (app scwm preferences)
	       (app scwm auto-raise)
	       )
  (scwm-options-dialog))

(use-modules (app scwm defoption))
(scwm-option-get *default-auto-raise-focus-proc*)

