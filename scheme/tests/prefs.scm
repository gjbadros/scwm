;; $Id$

(use-modules (app scwm defoption)
	     (app scwm primopts)
	     (app scwm preferences)
	     (app scwm themes)
	     (app scwm menus-extras)
	     (app scwm prompt-proc)
	     (app scwm base)
	     (app scwm face)
	     (app scwm fvwm-compat)
	     (app scwm fvwm-module)
	     (gtk gtk)
	     (gtk gdk))

(use-modules (app scwm gnome-hints))
(popup-menu (scwm-options-menu) #t)

(scwm-options-dialog)
(use-modules (app scwm auto-raise))
;; (window-style "*" #:smart-placement #f #:random-placement #f)

(scwm-option-range '*window-font*)

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

(use-modules 
	     (app scwm auto-raise)
	     (app scwm themes)
	     (app scwm preferences)
	     (app scwm primopts)
	     (app scwm prompt-proc)
	     (gtk gtk)
	     )
(scwm-options-dialog)

(use-modules (app scwm defoption))
(scwm-option-get *default-auto-raise-focus-proc*)

(define w (gtk-proc-selection-new "foo"))

(define c (gtk-proc-selection-clist-widget w))
(gtk-proc-selection-get-procedure w)
(gtk-proc-selection-get-procname w)

(begin
  (define row (make-vector 1))
  (vector-set! row 0 0)
  (define col (make-vector 1))
  (vector-set! col 0 1)
  (define text (make-vector 1))
  (vector-set! text 0 ""))

(gtk-clist-get-text c 1 0 text)
(gtk-clist-get-selection-info c 1 0 row col)

(set-reset-on-segv! 0)

(gtk-widget-destroy (car w))
(apropos-internal-with-modules ".")

(define-scwm-option *a-proc* resize-window
  "A proc." 
  #:type 'proc
  #:group 'test
  )

(gui-set '*a-proc*)

(procedure-source *a-proc*)
(procedure-name *a-proc*)
