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
  :use-module (app scwm sort)   ;; GJB:FIXME:G1.4: not needed in guile-1.4
  :use-module (app scwm defoption)
  :use-module (app scwm menus-extras)
  :use-module (app scwm message-window)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm prompt-bool)
  :use-module (app scwm prompt-string)
  :use-module (app scwm prompt-range)
  :use-module (app scwm prompt-font)
  :use-module (app scwm prompt-file)
  :use-module (app scwm prompt-color)
  )

;; (use-modules (gtk gtk))
;; (use-modules (app scwm preferences))
;; (use-modules (app scwm prompt-string))

(define-public mw-curval (make-message-window-clone-default ""))
(define-public mw-docs (make-message-window-clone-default ""))
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
;; (scwm-option-documentation '*use-scwm-system-proc*)

;; (show-current-value '*desk-width*)
;; (string-index "foo" #\b)
(define-public (scwm-option-short-documentation sym)
  (let* ((doc (scwm-option-documentation sym))
	 (i (string-index doc #\newline)))
    (if i
	(make-shared-substring doc 0 i)
	doc)))
  

(define (show-current-value sym)
  (message-window-set-message! mw-curval (printable (scwm-option-symget sym)))
  (apply message-window-set-position! mw-curval (map (lambda (x) (- x 10)) (pointer-position)))
  (message-window-show! mw-curval))

(define-public (popup-docs-for var)
  (message-window-set-message! mw-docs (scwm-option-short-documentation var))
  (apply message-window-set-position! mw-docs (map (lambda (x) (- x 40)) (pointer-position)))
  (message-window-show! mw-docs))

(define-public (toggle-docs-for var)
  (let ((docs (scwm-option-short-documentation var)))
    (if (and (message-window-visible? mw-docs) (string=? docs (message-window-message mw-docs)))
	(message-window-hide! mw-docs)
	(popup-docs-for var))))

;;(path-list->string-with-colons (scwm-option-get *theme-path*))
;; (use-modules (app scwm string-prompt))
;; (use-modules (gtk gtk))
;; (gui-set '*desk-width*)
(define-public (gui-set sym)
  (let* ((name (scwm-option-name sym))
	 (value (scwm-option-symget sym))
	 (type (scwm-option-type sym))
	 (range (scwm-option-range sym))
	 (favorites (scwm-option-favorites sym))
	 (var (eval sym))
	 (prompt (string-append "Set " name))
	 (title (string-append "Set " name))
	 (set-proc (lambda (v) (scwm-option-symset! sym v))))
    (case type
      ('string 
       (prompt-string prompt set-proc #:initval value #:title title))
      ((command file directory)
       (prompt-file prompt set-proc #:initval value #:title title #:favorites favorites))
      ('path
       (prompt-string prompt (lambda (v) (set-proc (string-with-colons->path-list v)))
		      #:initval (path-list->string-with-colons value)
		      #:title title))
      ('proc
       (prompt-string prompt (lambda (v) (set-proc (eval (string->symbol v))))
		      #:initval (symbol->string (procedure-name value))
		      #:title title))
      ('integer 
       (prompt-integer-range prompt range set-proc #:initval value #:title title))
      ('color 
       (prompt-color prompt set-proc #:initval value #:title title #:favorites favorites))
      ('real
       (prompt-range prompt range set-proc #:initval value #:title title))
      ((percent percent-or-on-off) ;; GJB:FIXME:: separate percent-or-on-off
       (prompt-integer-range prompt '(0 . 100) set-proc #:initval value #:title title))
      ('boolean
       (prompt-bool prompt set-proc #:initval value #:title title))
      (else
       (error "Cannot yet handle type " (symbol->string type))))))

(define-public (prompt-from-name name)
  (make-shared-substring name 1 (- (string-length name) 1)))

;;(prompt-from-name "*foo*")

(define-public (option-widget-and-getter sym)
  (let* ((name (scwm-option-name sym))
	 (value (scwm-option-symget sym))
	 (type (scwm-option-type sym))
	 (range (scwm-option-range sym))
	 (favorites (scwm-option-favorites sym))
	 (var (eval sym))
	 (prompt (prompt-from-name name)))
    (case type
      ('string
       (prompt-string-hbox prompt value))
      ((command directory file)
       (prompt-file-hbox prompt value favorites))
      ('path
       (prompt-path-hbox prompt value))
      ('proc
       (prompt-proc-hbox prompt value))
      ('font
       (prompt-font-hbox prompt value))
      ('color
       (prompt-color-hbox prompt value favorites))
      ('integer 
       (prompt-integer-range-hbox prompt range value))
      ('real
       (prompt-range-hbox prompt range value))
      ((percent percent-or-on-off) ;; GJB:FIXME:: separate percent-or-on-off
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
    (let ((doc (scwm-option-short-documentation sym))
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
;; (define tooltip (gtk-tooltips-new))

(define-public (scwm-options-dialog)
  "Popup a scwm options dialog box.
You may need to use-modules all the modules that contain
scwm-options to have this work for now."
  (let* ((toplevel (gtk-window-new 'toplevel))
	 (tooltip (gtk-tooltips-new))
	 (vbox (gtk-vbox-new #f 0))
	 (hbox (gtk-hbox-new #f 10))
	 (nb (scwm-options-notebook))
	 (cancelbut (gtk-button-new-with-label "Dismiss"))
	 )
    (gtk-window-set-title toplevel "Scwm Options")
    (gtk-box-pack-start hbox cancelbut #t #t)
    (gtk-widget-show cancelbut)
    (gtk-box-pack-start vbox nb #t #t)
    (gtk-widget-show vbox)
    (gtk-box-pack-start vbox hbox #t #t)
    (gtk-widget-show hbox)
    (gtk-container-add toplevel vbox)
    (gtk-tooltips-enable tooltip)
    (let ((pp (pointer-position)))
      (gtk-widget-set-uposition toplevel (- (car pp) 150) (cadr pp)))
    (gtk-widget-show toplevel)
    (gtk-signal-connect cancelbut "pressed"
			(lambda ()
			  (message-window-hide! mw-docs)
			  (gtk-widget-destroy toplevel)))
    (lambda ()
      (gtk-widget-hide toplevel)
      (gtk-widget-destroy toplevel))))

(define-public (sort-options-by-type symlist)
  (sort symlist (lambda (a b) (string<? (symbol->string (scwm-option-type a))
					(symbol->string (scwm-option-type b))))))

;;(sort-options-by-type scwm-options)


(define-public (scwm-options-notebook)
  (let* ((nb (gtk-notebook-new))
	 (sorted-options (sort (map (lambda (s) (cons (scwm-option-group s) s)) scwm-options)
			       (lambda (a b) (string>? (symbol->string (car a))
						       (symbol->string (car b))))))
	 (by-group (split-list-by-group sorted-options)))
    (map (lambda (page) (scwm-options-notebook-page nb (symbol->string (car page)) 
						    (sort-options-by-type (cdr page))))
	 by-group)
    (gtk-widget-show nb)
    nb))


(define-public (scwm-options-notebook-page nb title syms)
  "Popup a scwm options dialog box.
NOTE: Not quite functional, but I'm outta time!
GJB:FIXME::."
  (let* ((label (gtk-label-new title))
	 (vbox (gtk-vbox-new #f 10))
	 (hbox (gtk-hbox-new #t 50))
	 (widgets-and-applyers
	  (map (lambda (s) 
		 (let* ((widget-and-getter (option-widget-and-getter s))
			(widget (car widget-and-getter)))
		   (list widget
			 (lambda () 
			   (scwm-option-symset! 
			    s ((cadr widget-and-getter))))
			 s)))
	       syms))
	 (widgets (map (lambda (x) (let ((hbox (car x))
					 (helpbut (gtk-button-new-with-label "Help"))
					 (sym (caddr x)))
				     (gtk-signal-connect helpbut "pressed"
							 (lambda () 
							   (toggle-docs-for sym)))
				     (gtk-widget-show helpbut)
				     (gtk-box-pack-start hbox helpbut #f #f 20)
				     hbox)) widgets-and-applyers))
	 (applyers (map cadr widgets-and-applyers))
	 (apply-action (lambda ()
		  (for-each (lambda (a) (a)) applyers)))
	 (applybut (gtk-button-new-with-label "Apply"))
;;	 (okbut (gtk-button-new-with-label "Ok"))
;;	 (cancelbut (gtk-button-new-with-label "Cancel"))
	 )
    (map (lambda (w) (gtk-box-pack-start vbox w #t #f) (gtk-widget-show w))
	 widgets)
    (map (lambda (w) (gtk-box-pack-start hbox w #t #t) (gtk-widget-show w))
	 (list applybut))
    (gtk-widget-show hbox)
    (gtk-box-pack-start vbox hbox #f 0)
    (gtk-widget-show vbox)
    (gtk-signal-connect applybut "pressed" 
			(lambda () (apply-action)))
;;    (gtk-signal-connect okbut "pressed" 
;;			(lambda () 
;;			  (apply-action)
;;			  (message-window-hide! mw-docs)))
;;    (gtk-signal-connect cancelbut "pressed"
;;			(lambda ()
;;			  (message-window-hide! mw-docs)))
    (gtk-widget-show label)
    (gtk-notebook-append-page nb vbox label)
    nb))
