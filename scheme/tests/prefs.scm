;; $Id$

(use-modules (app scwm defoption)
	     (app scwm primopts)
	     (app scwm preferences)
	     (app scwm themes)
	     (app scwm menus-extras)
	     (gtk gtk))


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

