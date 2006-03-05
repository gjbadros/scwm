;;;; $Id$
;;;; Copyright (C) 1999, 2000 Greg J. Badros
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
  :use-module (app scwm sort)   ;; GJB:FIXME:G1.3.2: not needed in guile-1.3.2
  :use-module (app scwm defoption)
  :use-module (app scwm menus-extras)
  :use-module (app scwm message-window)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm prompt-bool)
  :use-module (app scwm prompt-string)
  :use-module (app scwm prompt-range)
  :use-module (app scwm prompt-enum)
  :use-module (app scwm prompt-font)
  :use-module (app scwm prompt-file)
  :use-module (app scwm prompt-color)
  :use-module (app scwm prompt-proc))


;; (use-modules (gtk gtk))
;; (use-modules (app scwm preferences))
;; (use-modules (app scwm prompt-string))

;;(define-scwm-group about "About" #:widget  ... ) ;;; GJB:FIXME::



(define-public mw-curval (make-message-window-clone-default ""))
(define-public mw-docs (make-message-window-clone-default ""))
(message-window-style mw-curval #:fg "black" #:bg "yellow") ;; old
(message-window-style mw-docs #:fg "black" #:bg "grey70")

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

(define-public (choices-from-favorites fav)
  (map (lambda (f) (cons f (symbol->string f))) fav))

;;(path-list->string-with-colons (optget *theme-path*))
;; (use-modules (app scwm prompt-string))
;; (use-modules (gtk gtk))
;; (gui-set '*desk-width*)
;; (gui-set '*default-focus-style*)
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
      ((command file directory sound)
       (prompt-file prompt set-proc #:initval value #:title title #:favorites favorites))
      ('path
       (prompt-string prompt (lambda (v) (set-proc (string-with-colons->path-list v)))
		      #:initval (path-list->string-with-colons value)
		      #:title title))
      ('proc
       (prompt-proc prompt set-proc #:initval value #:title title))
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
      ('enum
       (prompt-enum prompt (choices-from-favorites favorites) set-proc #:initval value #:title title))
      (else
       (error "Cannot yet handle type " (symbol->string type))))))

;;(prompt-from-name "*foo*")

(define-public (dummy-prompt-hbox prompt initval)
  "Create and return a dummy-prompting hbox and entry.
PROMPT is the prompt, and INITVAL is the initial string.
The returned value is a list: (hbox getter entry).
See also `prompt-string'."
  (let* ((hbox (gtk-hbox-new #f 0))
	 (entry (gtk-entry-new))
	 (entry-init "dummy")
	 (label (gtk-label-new prompt)))
    (gtk-entry-set-text entry entry-init)
    (gtk-box-pack-start hbox label #f #f 10)
    (gtk-box-pack-start hbox entry #t #t)
    (gtk-widget-set-usize entry (min 450 (max 100 (* 8 (string-length entry-init)))) 30)
    (gtk-widget-show-all hbox)
    (list hbox (lambda () initval) entry)))

;; (option-widget-and-getter '*nonant-highlight-color*)
;; (scwm-option-type '*nonant-highlight-color*)
(define-public (option-widget-and-getter sym)
  (let* ((name (scwm-option-name sym))
	 (value (scwm-option-symget sym))
	 (type (scwm-option-type sym))
	 (range (scwm-option-range sym))
	 (favorites (scwm-option-favorites sym))
	 (prompt name))
    (case type
      ('string
       (prompt-string-hbox prompt value))
      ((command directory file sound)
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
      ('enum
       (prompt-enum-hbox prompt (choices-from-favorites favorites) value))
      (else
       (display "Cannot yet handle type ")
       (display (symbol->string type))
       (newline)
       (dummy-prompt-hbox prompt value)))))


;; (gui-set '*desk-width*)   ;; an integer
;; (gui-set '*edge-x-scroll*) ;; a percent
;; (gui-set '*auto-raise*)   ;; a boolean
;; (gui-set '*theme-path*)   ;; a path (uses prompt-string for now)

(define*-public (option-menu sym)
  (let ((doc (scwm-option-short-documentation sym))
	(fav (scwm-option-favorites sym))
	(group (scwm-option-group sym))
	(type (scwm-option-type sym))
	(name (scwm-option-name sym))
	(range (scwm-option-range sym)))
    (menu (append
	   (list
	    (menuitem (string-append name " = " (printable (scwm-option-symget sym))) 
		      #:hover-action (lambda () (popup-docs-for sym))
		      #:unhover-action (lambda () (message-window-hide! mw-docs))
		      #:action noop ;; need this to make the hover-action work
		      ;; GJB:FIXME:: need a menu popdown proc as part of menu object
		      )
	    menu-separator
	    (menuitem "Set..." 
		      #:action (lambda () (gui-set sym))))
	   (if fav
	       (map (lambda (val) (menuitem (printable val)
					    #:action (lambda () (scwm-option-symset! sym val))))
		    (scwm-option-favorites sym))
	       '()))
	  #:hover-delay 100)))

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
;; (optget *edge-x-scroll*)

(define-public (scwm-options-menu)
  (menu (append
	 (list
	  (menu-title "All options")
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

(define-scwm-group preferences "Preferences")

(define-scwm-option *preferences-use-notebook* #f
  "If #t, use a notebook GUI for the Scwm options dialog, else use a listbox format."
  #:type 'boolean
  #:group 'preferences)
;; (set! *preferences-use-notebook* #f)

(define-public (scwm-options-dialog)
  "Popup a scwm options dialog box.
You may need to use-modules all the modules that contain
scwm-options to have this work for now.
Put \"(load-preferences)\" in your .scwmrc where you
want the saved preferences to take effect."
  (let* ((toplevel (gtk-window-new 'toplevel))
	 (tooltip (gtk-tooltips-new))
	 (vbox (gtk-vbox-new #f 0))
	 (hbox (gtk-hbox-new #f 10))
	 (done-action (lambda ()
			(save-preferences)
			(message-window-hide! mw-docs)
			(gtk-widget-destroy toplevel)))
	 (cancel-action (lambda ()
			(message-window-hide! mw-docs)
			(gtk-widget-destroy toplevel)))
	 (nb ((if *preferences-use-notebook* 
		  scwm-options-notebook
		  scwm-options-listbook) done-action cancel-action))
	 )
    (gtk-window-set-title toplevel "Scwm Options")
    (gtk-box-pack-start vbox nb #t #t)
    (gtk-box-pack-start vbox hbox #t #t)
    (gtk-container-add toplevel vbox)
    (gtk-tooltips-enable tooltip)
    (let ((pp (pointer-position)))
      (gtk-widget-set-uposition toplevel (- (car pp) 150) (cadr pp)))
    (gtk-widget-show-all toplevel)
    (lambda ()
      (gtk-widget-hide toplevel)
      (gtk-widget-destroy toplevel))))

(define-public (sort-options-by-type symlist)
  (sort symlist (lambda (a b) (string<? (symbol->string (scwm-option-type a))
					(symbol->string (scwm-option-type b))))))

;;(sort-options-by-type scwm-options)

(define-public (scwm-options-notebook done-action cancel-action)
  (let* ((nb (gtk-notebook-new))
	 (sorted-options (sort (map (lambda (s) (cons (scwm-option-group s) s)) scwm-options)
			       (lambda (a b) (string>? (scwm-group-name (car a))
						       (scwm-group-name (car b))))))
	 (by-group (split-list-by-group sorted-options)))
    (for-each (lambda (page)
		(let* ((title (scwm-group-name (car page)))
		       (vbox (scwm-options-vbox-page nb done-action 
						     cancel-action title 
						     (sort-options-by-type (cdr page)))))
		  (gtk-notebook-append-page nb vbox
					    (gtk-label-new title))))
	      by-group)
    (gtk-widget-show-all nb)
    nb))

(define ui-box-spacing 4)
(define ui-box-border 5)

(define-public (scwm-options-listbook done-action cancel-action)
  (let* ((hbox (gtk-hpaned-new))
	 (clist (gtk-clist-new 1))
	 (frame (gtk-frame-new ""))
	 (sorted-options (sort (map (lambda (s) (cons (scwm-option-group s) s)) scwm-options)
			       (lambda (a b) (string>? (scwm-group-name (car a))
						       (scwm-group-name (car b))))))
	 (by-group (split-list-by-group sorted-options))
	 (contents (make-vector (length by-group))))
    (gtk-container-border-width hbox ui-box-border)
    (gtk-clist-set-column-auto-resize clist 0 #t)
    (gtk-paned-add1 hbox clist)
    (gtk-paned-add2 hbox frame)
    (gtk-clist-set-selection-mode clist 'browse)
    (for-each (lambda (page)
		(let* ((title (scwm-group-name (car page)))
		       (vbox (scwm-options-vbox-page frame done-action 
						     cancel-action title 
						     (sort-options-by-type (cdr page))))
		       (row (gtk-clist-append clist (vector title))))
		  (gtk-widget-show-all vbox)
		  (vector-set! contents row (cons page vbox))))
	      by-group)
    (gtk-signal-connect clist "select_row"
			(lambda (row col event)
			  (listbook-select-row contents row frame)))
    (gtk-clist-select-row clist 0 0)
    (gtk-widget-show-all hbox)
    hbox))

(define (listbook-select-row contents row frame)
  (for-each (lambda (widget)
	      (gtk-container-remove frame widget))
	    (gtk-container-children frame))
  (let ((page (car (vector-ref contents row)))
	(widget (cdr (vector-ref contents row))))
    (gtk-frame-set-label frame (scwm-group-name (car page)))
    (gtk-container-add frame widget)))



(define-public (scwm-options-vbox-page parent done-action cancel-action title syms)
  "Popup a scwm options dialog box."
  (let* ((vbox (gtk-vbox-new #f 10))
	 (tooltip (gtk-tooltips-new))
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
	 (widgets (map (lambda (x)
			 (let ((hbox (car x))
			       (sym (caddr x)))
			   (define (frob c)
			     (for-each
			      (lambda (w)
				(if (gtk-container? w)
				    (frob w))
				(gtk-tooltips-set-tip
				 tooltip w
				 (scwm-option-short-documentation
				  sym)
				 ""))
			      (gtk-container-children c)))
			   (frob hbox)
			   hbox)) widgets-and-applyers))
	 (applyers (map cadr widgets-and-applyers))
	 (apply-action (lambda ()
			 (for-each (lambda (a) (a)) applyers)))
	 (applybut (gtk-button-new-with-label "Apply"))
	 (okbut (gtk-button-new-with-label "Ok"))
	 (cancelbut (gtk-button-new-with-label "Dismiss"))
	 (hbuttonbox (gtk-hbutton-box-new))
	 )
    (map (lambda (w) (gtk-box-pack-start vbox w #t #f))
	 widgets)
    (map (lambda (w) (gtk-box-pack-start hbuttonbox w #t #t))
	 (list okbut applybut cancelbut))
    (gtk-box-pack-start hbox hbuttonbox #t #t)
    (gtk-box-pack-start vbox hbox #f 0)
    (gtk-button-box-set-layout hbuttonbox 'spread)
    (gtk-widget-show-all vbox)
    (gtk-signal-connect applybut "clicked" 
			(lambda () (apply-action)))
    (gtk-signal-connect okbut "clicked" (lambda () (apply-action) (done-action)))
    (gtk-signal-connect cancelbut "clicked" cancel-action)
    vbox))


(define (read-color c port)
  (let ((s (read port))
	(b (read port)))
    (if (not (string=? b ">"))
	(error "Error reading color -- missing '>'")
    (make-color s))))


(define (read-font c port)
  (let ((s (read port))
	(b (read port)))
    (if (not (string=? b ">"))
	(error "Error reading font -- missing '>'")
    (make-font s))))

(define (read-proc c port)
  (define (last-char s)
    (if (string? s)
	(let ((l (string-length s)))
	  (substring s (- l 1) l))
	""))
  (define (as-string s)
    (if (symbol? s)
	(symbol->string s)
	s))
  (let* ((s (read port))
	 (st (symbol->string s))
	 (b st))
    (while (and (not (eof-object? b)) (not (string=? (last-char b) ">")))
	   (set! b (as-string (read port))))
    (if (or (eof-object? b) (not (string=? (last-char b) ">")))
	(error "Error reading procedure -- missing '>'"))
    (if (string=? (last-char st) ">")
	(set! s (string->symbol (substring st 0 (- (string-length st) 1))))
	(set! s (string->symbol s)))
    s))

(read-hash-extend #\< (lambda (c port)
			(case (read port)
			  ((color) (read-color c port))
			  ((font) (read-font c port))
			  ((primitive-procedure procedure) (read-proc c port))
			  (else (error "Bad primitive object.")))))

;; #<color "red">  ;; use scwm-eval-region-- scwm-last-sexp
;; will not do the right thing because there are no parens
;; (perhaps it should be extended to try to guess better?)
;;
;; #<font "-adobe-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*">
;;
;; #<primitive-procedure raise-window >
;; #<primitive-procedure raise-window>
;; 
;;
;; #<procedure icon-viewport-position lambda*:G11>

(define*-public (scwm-options-string #:optional (syms scwm-options))
  "Return a string that, when evalled, resets preferences to current values.
SYMS is a list of all the preference settings you wish to be included in
the string.  It defaults to the value of `scwm-options', the list of all
the options in the current session.  At present this means that if some
modules are not loaded, those options will not be included.
The returned string will contain multiple S-expressions, one for each Scwm preference
value."
  (apply string-append
       (map (lambda (s) 
	      (let ((module-name (with-output-to-string 
				   (lambda () (display (scwm-option-module s))))))
		(string-append 
		 "(eval-after-load '" module-name
		 " (lambda ()\n"
		 "  (scwm-option-set! " (symbol->string s) " "
		 (with-output-to-string 
		   (lambda ()
		     (let ((value (scwm-option-symget s)))
		       (if (or (list? value) (symbol? value)) (display "'"))
		       (write value))))
		 ")))\n")))
	    syms)))
	       

;; (scwm-options-string scwm-options)
;; (scwm-options-string)

;; (eval (scwm-options-string scwm-options))

(define*-public (save-preferences #:optional 
				 (filename (string-append (getenv "HOME") "/.scwm-options")))
  "Save the current Scwm preferences to FILENAME.
FILENAME defaults to $HOME/.scwm-options if not specified.
The string written is generated by `scwm-options-string'."
  (let ((fd (open-output-file filename)))
    (display (string-append ";; -*- scwm -*-\n" (scwm-options-string))
	     fd)
    (close-port fd)))
;; (save-preferences)
  
(define*-public (load-preferences #:optional 
				 (filename (string-append (getenv "HOME") "/.scwm-options")))
  "Load FILENAME to restore the Scwm preferences.
Since the options file is just a bunch of S-expressions,
this currently just `load's the file to evaluate it.
Returns #t iff the load was successful, false otherwise."
  (if (access? filename R_OK)
      (begin (load filename) #t)
      #f))
;; (load-preferences)
;; (load-preferences "notexisting")

;; (scwm-options-dialog)
