;; $Id$
;; (C) 1999 Greg J. Badros

(define-module (app scwm ui-constraints-toggle-menu)
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
     (make-menuitem "Constraints" #f "Enabled?" #f #f #f #f #f)
     menu-title
     (make-menuitem "Disable all constraints" disable-all-constraints #f #f #f #f #f "d")
     (make-menuitem "Enable all constraints" enable-all-constraints #f #f #f #f #f "e")
     menu-separator)
    (map (lambda (x)
	   (let* ((class (ui-constraint-class x))
		  (name (ui-constraint-class-name class))
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
			      (flash-windows-of-constraint cn)
			      (draw-constraint x))
			    (lambda () 
			      (undraw-constraint x)
			      (unflash-windows-of-constraint cn))
			    #f)))
	 global-constraint-instance-list))))

(define-public (popup-ui-constraints-toggle-menu)
  "Popup a menu that can be used to turn added constraints on and off.
See also `constraint-toggle-menu'."
  (popup-menu (ui-constraints-toggle-menu) #t))
