;;;; $Id$
;;;; (C) 1999 Greg J. Badros
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;;

(define-module (app scwm preferences)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm file)
  :use-module (app scwm defoption)
  :use-module (app scwm message-window)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm prompt-string)
  :use-module (app scwm prompt-bool)
  :use-module (app scwm prompt-range)
  )

;; (use-modules (app scwm preferences))
;; (use-modules (app scwm prompt-string))

(define mw-curval (make-message-window-clone-default ""))
(define mw-docs (make-message-window-clone-default ""))
(message-window-style mw-curval #:fg "black" #:bg "yellow") ;; old
(message-window-style mw-docs #:fg "black" #:bg "grey70")

;; with-output-to-string isn't working for me
(define (printable var)
  (cond ((number? var) (number->string var))
	((string? var) var)
	((boolean? var) (if var "True" "False"))
	;; GJB:FIXME:: lists are broken -- hardcoded for paths
	((list? var) (path-list->string-with-colons var))
	(t (error ("Printable cannot handle this value.")))))
;; (printable *theme-path*)
;; (string-with-colons->path-list (printable *theme-path*))

;; (show-current-value '*desk-width*)
(define (show-current-value sym)
  (message-window-set-message! mw-curval (printable (scwm-option-symget sym)))
  (apply message-window-set-position! mw-curval (map (lambda (x) (- x 10)) (pointer-position)))
  (message-window-show! mw-curval))

(define-public (popup-docs-for var)
  (message-window-set-message! mw-docs (scwm-option-documentation var))
  (apply message-window-set-position! mw-docs (map (lambda (x) (- x 40)) (pointer-position)))
  (message-window-show! mw-docs))

;;(path-list->string-with-colons (scwm-option-get *theme-path*))

;; (use-modules (app scwm string-prompt))
;; (use-modules (gtk gtk))
;; (gui-set '*desk-width*)
(define-public (gui-set sym)
  (let* ((name (scwm-option-name sym))
	(value (scwm-option-symget sym))
	(type (scwm-option-type sym))
	(range (scwm-option-range sym))
	(var (eval sym))
	(prompt (string-append "Set " name))
	(title (string-append "Set " name))
	(set-proc (lambda (v) (scwm-option-symset! sym v))))
    (case type
      (('string 'directory)
       (prompt-string prompt set-proc #:initval value #:title title))
      ('path
       (prompt-string prompt (lambda (v) (set-proc (string-with-colons->path-list v)))
		      #:initval (path-list->string-with-colons value)
		      #:title title))
      ('integer 
       (prompt-integer-range prompt range set-proc #:initval value #:title title))
      ('real
       (prompt-range prompt range set-proc #:initval value #:title title))
      ('percent
       (prompt-integer-range prompt '(0 . 100) set-proc #:initval value #:title title))
      ('boolean
       (prompt-bool prompt set-proc #:initval value #:title title))
      (else
       (error "Cannot yet handle type " (symbol->string type))))))

(define-public (option-widget-and-getter sym)
  (let* ((name (scwm-option-name sym))
	 (value (scwm-option-symget sym))
	 (type (scwm-option-type sym))
	 (range (scwm-option-range sym))
	 (var (eval sym))
	 (prompt (string-append "Set " name)))
    (case type
      (('string 'directory)
       (prompt-string-hbox prompt value))
      ('path
       (prompt-path-hbox prompt value))
      ('integer 
       (prompt-integer-range-hbox prompt range value))
      ('real
       (prompt-range-hbox prompt range value))
      ('percent
       (prompt-integer-range-hbox prompt '(0 . 100) value))
      ('boolean
       (prompt-bool-hbox prompt value))
      (else
       (error "Cannot yet handle type " (symbol->string type))))))


;; (gui-set '*desk-width*)   ;; an integer
;; (gui-set '*edge-x-scroll*) ;; a percent
;; (gui-set '*auto-raise*)   ;; a boolean
;; (gui-set '*theme-path*)   ;; a path (uses prompt-string for now)

(define*-public (option-menu sym)
  (let ((var (eval sym)))
    (let ((doc (scwm-option-documentation sym))
	  (fav (scwm-option-favorites sym))
	  (group (scwm-option-group sym))
	  (type (scwm-option-type sym))
	  (name (scwm-option-name sym))
	  (range (scwm-option-range sym))
	  (var (eval sym)))
      (menu (append
	     (list
	      (menuitem (string-append name " = " (printable (scwm-option-symget sym))) 
			#:hover-action (lambda () (popup-docs-for sym))
			#:unhover-action (lambda () (message-window-hide! mw-docs))
			#:action noop ;; need this to make the hover-action work
			;; GJB:FIXME:: need a menu popdown proc as part of menu object
			)
	      menu-title
	      (menuitem "Set..." 
			#:action (lambda () (gui-set sym))))
	     (if fav
		 (map (lambda (val) (menuitem (printable val)
					      #:action (lambda () (scwm-option-symset! sym val))))
		      (scwm-option-favorites sym))
		 '()))
	    #:hover-delay 100))))

(define-public (popup-option-menu sym)
  (popup-menu (option-menu sym) #t)
  (message-window-hide! mw-docs))

;; (option-menu '*desk-width*) (printable '*desk-width*)
;; (option-menu-helper '*desk-width*)
;; (popup-option-menu '*desk-width*)
;; (popup-option-menu *desk-width*) ;; ERROR! must use symbols
;; (popup-docs-for '*desk-width*)
;; (show-current-value '*desk-width*)
;; (message-window-hide! mw-docs)
;; (message-window-hide! mw-curval)
;; (gui-set '*desk-width*)
;; (gui-set '*edge-x-scroll*)
;; (gui-set '*theme-path*)
;; (scwm-option-get *edge-x-scroll*)

(define-public (scwm-options-menu)
  (menu (append
	 (list
	  (menuitem "All options" #f)
	  menu-title
	  menu-separator)
	 (map (lambda (sym)
		(let ((fav (scwm-option-favorites sym))
		      (itemtitle (symbol->string sym)))
		  (if fav
		      (menuitem itemtitle #:submenu (lambda () (option-menu sym)))
		      (menuitem itemtitle #:action (lambda () (gui-set sym))))))
	      scwm-options))))

;; (popup-menu (scwm-options-menu) #t)

;; (use-modules (app scwm preferences))
;; (use-modules (app scwm defoption))
;; (use-modules (app scwm primopts))
;; (scwm-options-dialog)
(define-public (scwm-options-dialog)
  "Popup a scwm options dialog box.
NOTE: Not quite functional, but I'm outta time!
GJB:FIXME::."
  (let* ((toplevel (gtk-window-new 'dialog))
	 (vbox (gtk-vbox-new #f 10))
	 (widgets-and-applyers
	  (map (lambda (s) 
		 (let ((widget-and-getter (option-widget-and-getter s)))
		   (cons (car widget-and-getter)
			 (lambda () 
			   (scwm-option-symset! 
			    s ((cadr widget-and-getter)))))))
	       scwm-options))
	 (widgets (map car widgets-and-applyers))
	 (applyers (map cdr widgets-and-applyers))
	 (apply-action (lambda ()
		  (for-each (lambda (a) (a)) applyers)))
	 (hbox (gtk-hbox-new 0 0))
	 (applybut (gtk-button-new-with-label "Apply"))
	 (okbut (gtk-button-new-with-label "Ok"))
	 (cancelbut (gtk-button-new-with-label "Cancel")))
    (gtk-window-set-title toplevel "Scwm Options")

    (map (lambda (w) (gtk-box-pack-start vbox w #t #t) (gtk-widget-show w))
	 widgets)
    (map (lambda (w) (gtk-box-pack-start hbox w #t #t) (gtk-widget-show w))
	 (list applybut okbut cancelbut))
    (gtk-widget-show hbox)
    (gtk-container-add toplevel vbox)
    (gtk-box-pack-start vbox hbox #t #t)
    (gtk-widget-show vbox)
    (let ((pp (pointer-position)))
      (gtk-widget-set-uposition toplevel (- (car pp) 150) (cadr pp)))
    (gtk-widget-show toplevel)
    (gtk-signal-connect applybut "pressed" 
			(lambda () (apply-action)))
    (gtk-signal-connect okbut "pressed" 
			(lambda () 
			  (apply-action)
			  (gtk-widget-destroy toplevel)))
    (gtk-signal-connect okbut "pressed" 
			(lambda () 
			  (gtk-widget-destroy toplevel)))
;;			  (proc (getter))))
    (gtk-signal-connect cancelbut "pressed"
			(lambda ()
			  (gtk-widget-destroy toplevel)))
    (lambda ()
      (gtk-widget-hide toplevel)
      (gtk-widget-destroy toplevel))))
