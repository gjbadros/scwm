;; $Id$
;; (C) 1999 Greg J. Badros

(define-module (app scwm ui-constraints-toggle-menu)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm ui-constraints))


;; (use-modules (app scwm ui-constraints-toggle-menu))
;; (load "/scratch/gjb/scwm/scheme/ui-constraints-toggle-menu")
;; (set-current-module the-root-module)

(define-public (ui-constraints-toggle-menu)
  "Return a menu of constraints that permits toggling their enabled? state."
  (menu
   (append
    (list
     (make-menuitem "Constraints" #f "Enabled?" #f #f #f #f #f)
     menu-title)
    (map (lambda (x)
	   (let* ((class (ui-constraint-class x))
		  (name (ui-constraint-class-name class))
		  (pixmap (ui-constraint-class-pixmap-name class))
		  (enabled? (ui-constraint-enabled? x)))
	     (make-menuitem name 
			    (if enabled?
				(lambda () (disable-ui-constraint x))
				(lambda () (enable-ui-constraint x)))
			    (if enabled? "Yes" "No")
			    #f #f
			    (lambda () (draw-constraint x))
			    (lambda () (undraw-constraint x))
			    #f)))
	 global-constraint-instance-list))))

(define-public (popup-ui-constraints-toggle-menu)
  "Popup a menu that can be used to turn added constraints on and off.
See also `constraint-toggle-menu'."
  (popup-menu (ui-constraints-toggle-menu) #t))
