;; $Id$
;; Copyright (C) 1999 Greg J. Badros

(define-module (app scwm ui-constraints-toggle-menu)
;;  :use-module (cassowary constraints)
  :use-module (app scwm base)
  :use-module (app scwm flux)
  :use-module (app scwm optargs)
  :use-module (app scwm ui-constraints))


;; (use-modules (app scwm ui-constraints-toggle-menu))
;; (load "/scratch/gjb/scwm/scheme/ui-constraints-toggle-menu.scm")
;; (set-current-module the-root-module)
;; (popup-ui-constraints-toggle-menu)

(define (flash-windows-of-constraint cn)
  (let ((color "red"))
    (for-each 
     (lambda (w) (flash-window w #:color color #:unflash-delay #f)) 
     (cl-windows-of-constraint cn))))

(define (unflash-windows-of-constraint cn)
  (for-each 
   (lambda (w) (unflash-window w))
   (cl-windows-of-constraint cn)))


(define-public (ui-constraints-toggle-menu)
  "Return a menu of constraints that permits toggling their enabled? state."
  (menu
   (append
    (list
     (menu-title "Constraints" #:extra-label "Enabled?")
     menu-separator
     (menuitem "&Disable all constraints" #:action disable-all-constraints)
     (menuitem "&Enable all constraints" #:action enable-all-constraints)
     menu-separator)
    (map (lambda (x)
	   (let* ((class (ui-constraint-class x))
		  (name ((ui-constraint-class-menuname-proc class) x))
		  (cn (ui-constraint-cn x))
;;		  (strength (cl-constraint-strength cn))
		  (pixmap (ui-constraint-class-pixmap-name class))
		  (enabled? (ui-constraint-enabled? x)))
	     (make-menuitem name 
			    (if enabled?
				(lambda () (disable-ui-constraint x))
				(lambda () (enable-ui-constraint x)))
			    (if enabled? "Yes" "No")
			    #f #f
			    (lambda ()
			      (map (lambda (c) (flash-windows-of-constraint c)) cn)
			      (draw-constraint x))
			    (lambda () 
			      (undraw-constraint x)
			      (map (lambda (c) (unflash-windows-of-constraint c)) cn))
			    #f)))
	 global-constraint-instance-list))))

(define-public (popup-ui-constraints-toggle-menu)
  "Popup a menu that can be used to turn added constraints on and off.
See also `constraint-toggle-menu'."
  (popup-menu (ui-constraints-toggle-menu) #t))
