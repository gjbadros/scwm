;;;; $Id$
;;;; Copyright (C) 1999 Greg J. Badros
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


(define-module (app scwm prompt-proc)
  :use-module (ice-9 session)
  :use-module (gtk gtk)
  :use-module (app scwm gtk)
  :use-module (app scwm base)
  :use-module (app scwm listops)
  :use-module (app scwm menus-extras)
  :use-module (app scwm optargs))



;;(use-modules (app scwm prompt-proc))
;;(define w (prompt-proc "Procedure?" (lambda (v) (display v) (newline)) #:initval move-window))
(define*-public (prompt-proc prompt proc #&key
			      (initval #f)
			      (title #f)
			      (favorites #f))
  "Prompt using PROMPT for a proc and call PROC with value if Ok is clicked.
INITVAL is a default initial proc as a string.
TITLE is a window title."
  (let* ((toplevel (gtk-window-new 'dialog))
	 (hbox-getter-and-entry (prompt-proc-hbox prompt initval favorites))
	 (hbox (car hbox-getter-and-entry))
	 (getter (cadr hbox-getter-and-entry))
	 (entry (caddr hbox-getter-and-entry))
	 (hbox-buttons (gtk-hbox-new #f 5))
	 (okbut (gtk-button-new-with-label "Ok"))
	 (cancelbut (gtk-button-new-with-label "Cancel")))
    (or title (set! title "prompt-proc"))
    (gtk-window-set-title toplevel title)
    (gtk-box-pack-start hbox-buttons okbut #t #t)
    (gtk-box-pack-start hbox-buttons cancelbut #t #t)
    (gtk-box-pack-start hbox hbox-buttons #t #t)
    (gtk-container-add toplevel hbox)
    (gtk-widget-show hbox-buttons)
    (gtk-widget-show cancelbut)
    (gtk-widget-show okbut)
    (let ((pp (pointer-position)))
      (gtk-widget-set-uposition toplevel (- (car pp) 150) (cadr pp)))
    (gtk-widget-show toplevel)
    (gtk-signal-connect okbut "pressed" 
			(lambda () 
			  (gtk-widget-destroy toplevel)
			  (proc (getter))))
    (gtk-signal-connect entry "activate" 
			(lambda () 
			  (gtk-widget-destroy toplevel)
			  (proc (getter))))
    (gtk-signal-connect cancelbut "pressed"
			(lambda ()
			  (gtk-widget-destroy toplevel)))
    (lambda ()
      (gtk-widget-hide toplevel)
      (gtk-widget-destroy toplevel))))

(define*-public (prompt-proc-hbox prompt initval #&optional favorites)
  "Create and return a proc-prompting hbox, complete with link to full proc dialog.
PROMPT is the prompt, INITVAL is the initial proc as a string.
The returned value is a list: (hbox getter entry).
See also `prompt-proc'."
  (let* ((hbox (gtk-hbox-new #f 0))
	 (cb (if (list? favorites) (gtk-combo-new) #f))
	 (entry (if cb (gtk-combo-entry cb) (gtk-entry-new)))
	 (selbut (gtk-button-new-with-label "Choose..."))
	 (procname (procedure-name initval))
	 (entry-init (if procname
			 (symbol->string procname)
			 "#f"))
	 (label (gtk-label-new (if (string? prompt) prompt "Proc?"))))
    (if cb
	(gtk-combo-set-popdown-strings cb
				       (list->vector favorites)))
    (gtk-entry-set-text entry entry-init)
    (gtk-box-pack-start hbox label #f #f 10)
    (gtk-box-pack-start hbox (or cb entry) #t #t)
    (gtk-box-pack-start hbox selbut #f #f 10)
    (gtk-widget-set-usize entry (min 450 (max 100 (* 10 (string-length entry-init)))) 30)
    (gtk-widget-show (or cb entry))
    (gtk-widget-show label)
    (gtk-widget-show selbut)
    (gtk-widget-show hbox)
    (gtk-signal-connect selbut "pressed"
			(lambda ()
			  (let* ((proc-selector (gtk-proc-selection-new "Procedure Selection Dialog"))
				 (dialog (gtk-proc-selection-toplevel-widget proc-selector)))
			    (gtk-signal-connect
			     (gtk-proc-selection-ok-button proc-selector)
			     "clicked" (lambda () 
					 (gtk-entry-set-text 
					  entry 
					  (gtk-proc-selection-get-procname proc-selector))
					 (gtk-widget-destroy dialog)))
			    (gtk-signal-connect
			     (gtk-proc-selection-cancel-button proc-selector)
			     "clicked" (lambda () (gtk-widget-destroy dialog)))
			    (gtk-widget-show dialog))))
    (list hbox (lambda () 
		 (let* ((procname (gtk-entry-get-text entry))
			(proc (eval (string->symbol procname))))
		   (if (procedure? proc) proc #f)))
	  entry)))

(define-public (gtk-proc-selection-new title)
  "Returns a new procedure-selecting dialog box."
  (let* ((toplevel (gtk-window-new 'dialog))
	 (titles #("Module" "Procedure"))
	 (vbox (gtk-vbox-new #f 0))
	 (hbox (gtk-hbox-new #f 10))
	 (clist (gtk-clist-new-with-titles titles))
	 (scrolled-win (gtk-scrolled-window-new))
	 (okbut (gtk-button-new-with-label "Ok"))
	 (cancelbut (gtk-button-new-with-label "Cancel"))
	 (sort-column 0)
	 (sort-ascending #t)
	 (selected-row -1)
	 (x #("Module" "Proc"))
	 )
    (gtk-window-set-title toplevel title)
    (gtk-container-add toplevel vbox)
    (gtk-scrolled-window-set-policy scrolled-win 'automatic 'automatic)
    (gtk-widget-set-usize scrolled-win 500 400)
    (gtk-container-add scrolled-win clist)
    (gtk-clist-set-selection-mode clist 'single)
    (gtk-clist-set-column-resizeable clist 0 #t)
    (gtk-clist-set-column-resizeable clist 1 #t)
    (gtk-clist-set-column-width clist 0 100)
    (gtk-clist-set-column-justification clist 0 'left)
    (gtk-clist-set-column-justification clist 1 'left)
    (for-each (lambda (mp) 
		(vector-set! x 0 (symbol->string (car mp)))
		(vector-set! x 1 (symbol->string (cdr mp)))
		(gtk-clist-append clist x))
	      (interactive-proc-list "."))
    (gtk-signal-connect clist "select_row" 
			(lambda (row col event)
			  (display "selected ")
			  (display row)
			  (display " x ")
			  (display col)
			  (newline)
			  (set! selected-row row)))
    (gtk-box-pack-start vbox scrolled-win #t #t 0)
    (gtk-box-pack-start vbox hbox #f #f 0)
    (gtk-box-pack-start hbox okbut #f #f 0)
    (gtk-box-pack-start hbox cancelbut #f #f 0)
    (map gtk-widget-show (list vbox hbox clist scrolled-win okbut cancelbut toplevel))
    (list toplevel okbut cancelbut clist 
	  ;; GJB:FIXME:: the number of columns (1) is a hack
	  (lambda () (if (>= selected-row 0)
			 (gtk-clist-get-row-values clist selected-row 1)
			 #f)))))

(define-public (gtk-proc-selection-toplevel-widget proc-dialog)
  "Returns the toplevel window widget from PROC-DIALOG."
  (list-ref proc-dialog 0))

(define-public (gtk-proc-selection-ok-button proc-dialog)
  "Returns the ok button from PROC-DIALOG."
  (list-ref proc-dialog 1))

(define-public (gtk-proc-selection-cancel-button proc-dialog)
  "Returns the cancel button from PROC-DIALOG."
  (list-ref proc-dialog 2))

(define-public (gtk-proc-selection-clist-widget proc-dialog)
  "Returns the clist widgetfrom PROC-DIALOG."
  (list-ref proc-dialog 3))

(define-public (gtk-proc-selection-get-procname proc-dialog)
  "Returns the currently selected procedure name from PROC-DIALOG."
  (let* ((m-p-proc (list-ref proc-dialog 4))
	 (m-p (m-p-proc)))
    (if m-p
	(cadr m-p)
	#f)))

(define-public (gtk-proc-selection-get-procedure proc-dialog)
  "Returns the currently selected procedure from PROC-DIALOG."
  (eval (string->symbol (gtk-proc-selection-get-procname proc-dialog))))

;; (use-modules (ice-9 session))
;; (use-modules (app scwm prompt-proc))
(define-public (interactive-proc-list match)
  (let* ((procs-and-modules (apropos-internal-with-modules match))
	 (only-interactive (filter 
			    (lambda (sym)
			      (let ((p (eval (cdr sym))))
				(and (procedure? p)
				     (equal? 
				      (procedure-property p 'arity)
				      (list 0 0 #t)))))
			    procs-and-modules))
;;	 (by-group (split-list-by-group procs-and-modules))
	 )
    only-interactive))

;;(interactive-proc-list "window")
;; (use-modules (app scwm listops))
;; (use-modules (app scwm message-window))
;; (use-modules (app scwm animation))
;;(procedure-property (eval (cdaddr (apropos-internal-with-modules "window"))) 'arity)
;;(apropos-internal-with-modules "window")

;; very slightly changed from ice-9 session's apropos-internal
(define-public (apropos-internal-with-modules rgx)
  "Return a list of accessible variable names."
  (let ((match (make-regexp rgx))
	(modules (cons (current-module)
		       (module-uses (current-module))))
	(recorded (make-vector 61 '()))
	(vars (cons '() '())))
    (let ((last vars))
      (for-each
       (lambda (module)
	 (for-each
	  (lambda (obarray)
	    (array-for-each
	     (lambda (oblist)
	       (for-each
		(lambda (x)
		  (if (and (regexp-exec match (car x))
			   (not (hashq-get-handle recorded (car x))))
		      (begin
			(set-cdr! last (cons (cons (module-name module) (car x)) '()))
			(set! last (cdr last))
			(hashq-set! recorded (car x) #t))))
		oblist))
	     obarray))
	  (if (or (eq? module the-scm-module)
		  (eq? module the-root-module))
	      (list (builtin-weak-bindings)
		    (builtin-bindings))
	      (list (module-obarray module)))))
       modules))
    (cdr vars)))

(define-public (gtk-clist-get-row-values clist row col)
  "Return a list of the COL columns of row ROW of CLIST."
  (let ((text (make-vector 1))
	(answer '()))
    (vector-set! text 0 "")
    (do ()
	((< col 0) answer)
      (gtk-clist-get-text clist row col text)
      (set! answer (cons (vector-ref text 0) answer))
      (set! col (- col 1)))))

(define-public (module-and-proc-names->proc module-name proc-name)
  "Return the procedure named PROC-NAME from module named MODULE-NAME.
PROC-NAME and MODULE-NAME are both strings.  Note that module name
must be a full name such as \"app scwm base\"."
  (module-ref (resolve-module (split-c-module-name module-name))
	      (string->symbol proc-name)))

;;(apply module-and-proc-names->proc (gtk-clist-get-row-values c 0 1))
;;(gtk-clist-get-row-values c 0 1)
;;(gtk-clist-get-row-values c 1 1)
;;(gtk-proc-selection-get-procedure w)
;;(string->symbol (cadr (gtk-proc-selection-get-procedure w)))
