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


;;; TODO
;;; click on column heading to sort on that column
;;;
;;; cache the information about bindings list and command list
;;; in local data structures so it's faster to re-sort, etc.
;;;
;;; Handle all-modifier key-bindings
;;; 
;;; Make the type option-menu show the current setting when a new binding is selected
;;;
;;; update the type option-menu to display the right pair of
;;; possibilities depending on the kind of event (mouse/key)



(define-module (app scwm prompt-binding)
  :use-module (ice-9 session)
  :use-module (ice-9 regex)
  :use-module (app scwm reflection)
  :use-module (app scwm doc)
  :use-module (gtk gtk)
  :use-module (app scwm gtk)
  :use-module (app scwm prompt-proc)
  :use-module (app scwm base)
  :use-module (app scwm stringops)
  :use-module (app scwm listops)
  :use-module (app scwm menus-extras)
  :use-module (app scwm optargs))

(define-public (clist-find clist cols pred)
  "Return the row number of the first row in CLIST that PRED answers #t for.
PRED is called with arguments that are strings of the first COLS columns of successive
rows of CLIST. Returns -1 if PRED never evaluates to #t."
  (let ((done #f)
	(row 0))
    (set! cols (- cols 1)) ;; use 1 for two columns
    (while (not done)
	   (let ((vals (gtk-clist-get-row-values clist row cols)))
	     (if (not (car vals))
		 (begin
		   (set! done #t)
		   (set! row -1))
		 (if (apply pred (gtk-clist-get-row-values clist row cols))
		     (set! done #t)
		     (set! row (+ 1 row))))))
    row))

(define-public (clist-set-row-text clist row cols)
  "Set the text of ROW of CLIST to COLS.
COLS is a list of strings."
  (let ((i 0))
    (for-each (lambda (val)
		(gtk-clist-set-text clist row i val)
		(set! i (1+ i)))
	      cols)))

(define ui-box-spacing 4)
(define ui-box-border 5)

(define-public contexts-and-descriptions
  '((all . "anywhere.")
    (root . "in only the root window (the background).")
    (window . "in the client window application area.")
    (title . "in the titlebar.")
    (icon . "when the pointer is over a minimized application icon.")
    (frame-corners . "when the pointer is over a corner of the frame.")
    (frame-sides . "when the pointer is over a side bar of the frame.")
    (left-button-1 . "when the pointer is in the leftmost button of the title bar (if any).")
    (left-button-2 . "when the pointer is in the 2nd leftmost button of the title bar (if any).")
    (left-button-3 . "when the pointer is in the 3rd leftmost button of the title bar (if any).")
    (left-button-4 . "when the pointer is in the 4th leftmost button of the title bar (if any).")
    (left-button-5 . "when the pointer is in the 5th leftmost button of the title bar (if any).")
    (right-button-1 . "when the pointer is in the rightmost button of the title bar (if any).")
    (right-button-2 . "when the pointer is in the 2nd rightmost button of the title bar (if any).")
    (right-button-3 . "when the pointer is in the 3rd rightmost button of the title bar (if any).")
    (right-button-4 . "when the pointer is in the 4th rightmost button of the title bar (if any).")
    (right-button-5 . "when the pointer is in the 5th rightmost button of the title bar (if any).")))

(define descr-prefix-string
  "Keymap containing bindings active ")

(define-public (raw-binding->key-descriptor binding)
  (apply 
   (lambda (mouse? context modmask keybut proc1 proc2)
     (if mouse?
	 (string-append (keymask->string modmask)
			"Button"
			(number->string keybut))
	 (string-append (keymask-keycode->string modmask keybut))))
   binding))

(define (binding-clist-set-row! x binding)
  (let* ((key (raw-binding->key-descriptor binding))
	 (proc (list-ref binding 4))
	 (proc2 (list-ref binding 5))
	 (procname (procedure->string proc))
	 (procname2 (procedure->string proc2))
	 (is-mouse (car binding)))
    (vector-set! x 0 key)
    (if is-mouse
	(begin
	  (if proc
	      (begin
		(vector-set! x 1 "Complex")
		(vector-set! x 2 procname)))
	  (if proc2
	      (begin
		(vector-set! x 1 "Immed")
		(vector-set! x 2 procname2))))
	;; not a mouse binding (i.e., key)
	(begin
	  (if proc
	      (begin
		(vector-set! x 1 "Press")
		(vector-set! x 2 procname)))
	  (if proc2
	      (begin
		(vector-set! x 1 "Release")
		(vector-set! x 2 procname2)))))))

(define (populate-clist-with-bindings-from clist context)
  (let ((x #("Key" "Type" "Command")))
    (for-each
     (lambda (binding)
       (binding-clist-set-row! x binding)
       (gtk-clist-append clist x))
     (sort!
      (lookup-procedure-bindings #f context)
      (lambda (a b)
	(string-ci<? (raw-binding->key-descriptor a)
		     (raw-binding->key-descriptor b)))))))
  
(define (populate-cmd-clist-with-procedures clist)
  (let ((x #("procname" "modulename")))
    (for-each (lambda (mp)
		(let ((proc (eval (cdr mp))))
		  (let ((modulename (symbol->string (car mp)))
			(procname (symbol->string (cdr mp))))
		    (vector-set! x 0 procname)
		    (vector-set! x 1 modulename)
		    (gtk-clist-append clist x))))
	      (sort!
	       (interactive-procedure-apropos-with-modules "")
	       (lambda (a b) 
		 (let ((modname-a (symbol->string (car a)))
		       (procname-a (symbol->string (cdr a)))
		       (modname-b (symbol->string (car b)))
		       (procname-b (symbol->string (cdr b))))
		   (string-ci<? procname-a procname-b)))))))


(define want-debug-clist-select #f)

(define-public (debug-clist-select clist-name row col event)
  (if want-debug-clist-select
      (begin
	(display clist-name) (display ": ")
	(display row) (display ", ") (display col)
	(newline))))

(define-public pb-cmd-clist #f)
(define current-type "unset")
(define current-context "all")

(define (make-type-option-menu types)
  (let ((menu (gtk-menu-new))
	(group #f))
    (for-each (lambda (text) 
		(let ((mi (gtk-radio-menu-item-new-with-label group text)))
		  (gtk-menu-append menu mi)
		  (gtk-signal-connect mi "activate"
				      (lambda ()
					(set! current-type text)))
		  (set! group mi)))
	      types)
    (gtk-widget-show menu)
    menu))

(define (make-button-type-option-menu)
  (make-type-option-menu (list "Immed" "Complex")))

(define (make-key-type-option-menu)
  (make-type-option-menu (list "Press" "Release")))

;; GJB:FIXME:: this does not do error checking
(define (add-remove-binding key type command mouse-proc key-proc)
  "MOUSE-PROC is either `bind-mouse' or `unbind-mouse'.
KEY-PROC is either `bind-key' or `unbind-key'."
  (let ((proc
	 (let ((m (string-match "^(.*-)Button([1-5])$" key) )
	       (cmd-sym (string->symbol command))
	       (context-sym (string->symbol current-context)))
	   (if m
	       (let ((prefix (match:substring m 1))
		     (butnum (match:substring m 2)))
		 (lambda ()
		   (mouse-proc context-sym
			       (string-append prefix butnum)
			       (if (not (string=? type "Immed"))
				   (eval cmd-sym) #f)
			       (if (string=? type "Immed")
				   (eval cmd-sym) #f)
			       )))
	       ;; not a mouse binding, so hopefully a keyboard one
	       (lambda ()
		 (key-proc context-sym
			   key 
			   (if (not (string=? type "Release"))
				(eval cmd-sym) #f)
			   (if (string=? type "Release")
			       (eval cmd-sym) #f)))))))
    (if (string=? key "Null") #f proc)))

(define (remove-binding-for-row clist row)
  (apply remove-binding (gtk-clist-get-row-values clist row 2)))

(define (remove-binding key type command)
  (let ((proc (add-remove-binding key type command unbind-mouse unbind-key)))
    (if proc (proc))))

(define (add-binding key type command)
  (add-remove-binding key type command bind-mouse bind-key))

;; (use-scwm-modules (ice-9 regex))
;; (begin (set! current-context "all") (add-binding "H-Button1" "Immed" "send-key-press-prior"))
;; (begin (set! current-context "all") (add-binding "H-Button1" "Complex" "send-key-press-prior"))
;; (begin (set! current-context "all") (add-binding "H-Button1" "foo" "send-key-press-prior"))
;; (begin (set! current-context "all") (add-binding "H-1" "bar" "send-key-press-prior"))
;; (begin (set! current-context "all") (add-binding "H-1" "Press" "send-key-press-prior"))
;; (begin (set! current-context "all") (add-binding "H-1" "Release" "send-key-press-prior"))
;; (begin (set! current-context "all") (add-binding "H-1" "bar" "send-key-press-prior"))
;; (begin (set! current-context "root") (add-binding "H-1" "bar" "send-key-press-prior"))
;; (define p (add-binding "H-1" "Press" "next-window"))  (p)
;; (list (procedure-source p) (procedure-environment p))

(define-public (prompt-binding-vbox)
  (let* ((name (symbol->string (caar contexts-and-descriptions)))
	 (description (string-append descr-prefix-string (cdar contexts-and-descriptions)))
	 (current-documented-command "")
	 (current-cmd-row #f)
	 (current-binding-row #f)
	 (context-frame (gtk-frame-new name))
	 (vbox (gtk-vbox-new #f 0))
	 (ins-del-bbox (gtk-hbutton-box-new))
	 (vbox-2 (gtk-vbox-new #f 0))
	 (context-label (gtk-label-new description))
	 (insert (gtk-button-new-with-label "Insert"))
	 (copy (gtk-button-new-with-label "Copy"))
	 (change (gtk-button-new-with-label "Change"))
	 (delete (gtk-button-new-with-label "Delete"))
	 (clist (gtk-clist-new-with-titles #("Key" "Type" "Command")))
	 (scroller (gtk-scrolled-window-new)))
    
    ;;(gtk-widget-set-state insert 'insensitive)
    ;;(gtk-widget-set-state copy 'insensitive)
    ;;(gtk-widget-set-state change 'insensitive)
    ;;(gtk-widget-set-state delete 'insensitive)

    (gtk-widget-set-usize context-label 400 32)
    (gtk-widget-set-usize scroller 400 250)
    (gtk-box-set-spacing vbox ui-box-spacing)
    (gtk-container-border-width vbox ui-box-border)
    (map (lambda (i) 
	   (gtk-clist-set-column-resizeable clist i #t)
	   (gtk-clist-set-column-justification clist i 'left))
	 (iota 3))
    (gtk-clist-set-column-width clist 0 120)
    (gtk-clist-set-column-width clist 1 50)
    (gtk-clist-set-column-width clist 2 180)
    (gtk-clist-set-selection-mode clist 'browse)
    (gtk-scrolled-window-set-policy scroller 'automatic 'automatic)
    (gtk-container-add vbox scroller)
    (gtk-box-pack-end vbox ins-del-bbox)
    (gtk-container-add scroller clist)
    (gtk-container-add ins-del-bbox insert)
    (gtk-container-add ins-del-bbox copy)
    (gtk-container-add ins-del-bbox change)
    (gtk-container-add ins-del-bbox delete)
    (populate-clist-with-bindings-from clist (caar contexts-and-descriptions))

    (gtk-box-pack-start vbox-2 context-label)
    (gtk-label-set-justify context-label 'left)
    (gtk-label-set-line-wrap context-label #t)
    (gtk-container-add vbox-2 vbox)
    (gtk-container-add context-frame vbox-2)
    (let*
	((hbox-1 (gtk-hbox-new #f 0))
	 (vbox (gtk-vbox-new #f 0))
	 (key (gtk-entry-new))
	 (button-type-option-menu (gtk-option-menu-new))
	 (key-type-option-menu (gtk-option-menu-new))
	 (button-type-menu (make-button-type-option-menu))
	 (key-type-menu (make-key-type-option-menu))
	 (entry (gtk-entry-new))
	 (grab-key-button (gtk-button-new-with-label "grab"))
	 (set-proc-button (gtk-button-new-with-label "set"))
	 (hbox-entry (gtk-hbox-new #f 4))
	 (hbox-key-name (gtk-hbox-new #f 0))
	 (hbox-entry-proc (gtk-hbox-new #f 0))
	 (map-clist (gtk-clist-new-with-titles #("Context")))
	 (doc-frame (gtk-frame-new "Procedure Documentation"))
	 (cmd-clist (gtk-clist-new-with-titles #("Commands" "Module")))
	 (scroller (gtk-scrolled-window-new))
	 (scroller-2 (gtk-scrolled-window-new))
	 (doc-textbox (gtk-text-new #f #f))
	 (scroller-doc (gtk-scrolled-window-new)))
;;      (set! pb-cmd-clist cmd-clist) ;; public for debugging
      ;;    (gtk-box-pack-start doc-frame scroller-doc)
      (gtk-option-menu-set-menu button-type-option-menu button-type-menu)
      (gtk-option-menu-set-menu key-type-option-menu key-type-menu)
      (gtk-box-set-spacing hbox-1 ui-box-spacing)
      (gtk-container-border-width hbox-1 ui-box-border)
      (gtk-box-set-spacing vbox ui-box-spacing)
      (gtk-container-border-width vbox ui-box-border)
      (gtk-scrolled-window-set-policy scroller-doc 'automatic 'automatic)

      ;; 1. the key and command editing widget
      (gtk-scrolled-window-set-policy scroller 'automatic 'automatic)
      (gtk-widget-set-usize scroller 200 100)
      (gtk-container-add scroller cmd-clist)
      (map (lambda (i) 
	     (gtk-clist-set-column-resizeable cmd-clist i #t)
	     (gtk-clist-set-column-justification cmd-clist i 'left))
	   (iota 2))
      (gtk-clist-set-column-width cmd-clist 0 120)
      (gtk-clist-set-column-width cmd-clist 1 120)

      (populate-cmd-clist-with-procedures cmd-clist)

      (gtk-widget-set-usize button-type-option-menu 80 15)
      (gtk-widget-set-usize key-type-option-menu 80 15)

      (gtk-container-add hbox-key-name key)
      (gtk-container-add hbox-key-name grab-key-button)
      (gtk-container-add hbox-entry-proc entry)
      (gtk-container-add hbox-entry-proc set-proc-button)
      (gtk-container-add hbox-entry hbox-key-name)
      (gtk-container-add hbox-entry key-type-option-menu)
      (gtk-container-add hbox-entry button-type-option-menu)
      (gtk-container-add hbox-entry hbox-entry-proc)


      (gtk-container-add vbox-2 hbox-entry)

      ;; 2. the context selection widget
      (gtk-scrolled-window-set-policy scroller-2 'automatic 'automatic)
      (gtk-clist-set-column-auto-resize map-clist 0 #t)
      (gtk-clist-set-selection-mode map-clist 'browse)
      (gtk-widget-set-usize scroller-2 80 100)
      (gtk-widget-set-usize scroller-doc 100 50)
      (gtk-container-add scroller-2 map-clist)
      (gtk-container-add hbox-1 scroller-2)
      (gtk-container-add scroller-doc doc-textbox)
      (gtk-container-add hbox-1 scroller)
      
      (let ((x #("")))
	(map (lambda (name)
	       (vector-set! x 0 name)
	       (gtk-clist-append map-clist x))
	     (map (lambda (i) (symbol->string (car i))) contexts-and-descriptions)))

      (gtk-clist-select-row map-clist 0 0)
      (gtk-container-add doc-frame scroller-doc)
      (gtk-container-add vbox context-frame)
      (gtk-container-add vbox hbox-1)
      (gtk-container-add vbox doc-frame)

      ;; the context list left, second from bottom
      (gtk-signal-connect map-clist "select_row"
			  ;; prompt-binding:select-context
			  (lambda (row col event)
			    (let ((sym-and-descr (list-ref 
						  contexts-and-descriptions
						  row)))
			      (set! current-context (symbol->string (car sym-and-descr)))
			      (gtk-frame-set-label context-frame current-context)
			      (gtk-label-set-text 
			       context-label 
			       (string-append descr-prefix-string 
					      (cdr sym-and-descr)))
			      (gtk-clist-clear clist)
			      (populate-clist-with-bindings-from
			       clist (car sym-and-descr))
			      )))

      ;; the command list right, second from bottom
      (gtk-signal-connect 
       cmd-clist "select_row"
       ;; prompt-binding:set-command
       (lambda (row col event)
	 (debug-clist-select "commands" row col event)
	 (let* ((m-p (gtk-clist-get-row-values cmd-clist row 2))
		(cmd (car m-p))
		(docs (and m-p (proc-doc (procedure-string->procedure cmd)))))
	   (set! current-cmd-row row)
	   (set! current-documented-command cmd)
	   (gtk-text-replace doc-textbox (if (string? docs) docs ""))
	   (gtk-scrolled-window-set-vadjustment-value scroller-doc 0)
	   (gtk-frame-set-label doc-frame cmd)
	 )))

      ;; the text entry just about the command list
      (gtk-signal-connect 
       entry "changed"
       ;; prompt-binding:set-event
       (lambda ()
	 (let* ((cmd (gtk-entry-get-text entry))
		(cmd-row (clist-find cmd-clist 2
				     (lambda (ccmd mod)
				       (string-ci-has-prefix ccmd cmd)))))
	   (if (and (>= cmd-row 0) (not (eqv? cmd-row current-cmd-row)))
	       (begin
		 (set! current-cmd-row cmd-row)
		 (gtk-clist-select-row cmd-clist cmd-row 0)
		 (gtk-clist-moveto cmd-clist cmd-row 0 .5 .5)))
	   )))

      ;; the insert button in the binding frame at the top
      (gtk-signal-connect insert "clicked" 
			  ;; prompt-binding:insert
			  (lambda ()
			    (let ((x #("Null" "Press" "noop")))
			      (gtk-clist-insert clist current-binding-row x)
			      (gtk-clist-select-row clist current-binding-row 0)
			      )))

      ;; the copy button in the binding frame at the top
      (gtk-signal-connect copy "clicked" 
			  ;; prompt-binding:copy
			  (lambda ()
			    (let* ((vals (gtk-clist-get-row-values clist current-binding-row 2)))
			      (set-car! vals "Null")
			      (gtk-clist-insert clist current-binding-row vals)
			      (gtk-clist-select-row clist current-binding-row 0))))

      ;; the change button in the binding frame at the top
      (gtk-signal-connect change "clicked" 
			  ;; prompt-binding:change
			  ;; get changes from
			  ;; key, 
			  ;; key-type-option-menu or button-type-option-menu, [in current-type]
			  ;; and entry (the proc name)
			  (lambda ()
			    (let* ((key-name (gtk-entry-get-text key))
				   (proc-name (gtk-entry-get-text entry))
				   (x #(key-name current-type proc-name)))
			      (let ((code-to-add-binding (add-binding key-name current-type proc-name)))
				;; above gives #f if the binding is no good
				(if code-to-add-binding
				    (let ((answer #f))
				      (remove-binding-for-row clist current-binding-row)
				      (catch #t
					     (lambda ()
					       (set! answer (code-to-add-binding)))
					     (lambda args
					       (caught-error "Error adding binding!\n" args)
					       (beep)))
				      (clist-set-row-text clist 
							  current-binding-row 
							  (list key-name current-type proc-name))
				      (if (not answer) 
					  (begin (beep) (gtk-entry-select-region key 0 -1))))
				    (beep))))))

      ;; the delete button in the binding frame at the top
      (gtk-signal-connect delete "clicked"
			  ;; prompt-binding:delete
			  (lambda ()
			    (remove-binding-for-row clist current-binding-row)
			    (gtk-clist-remove clist current-binding-row)
			    (gtk-clist-select-row clist current-binding-row 0)))

      ;; the "set" button next to the name of the procedure
      ;; that we've bound to the event
      (gtk-signal-connect set-proc-button "clicked"
			  (lambda ()
			    (gtk-entry-set-text entry current-documented-command)))

      ;; the "grab" button next to the event descriptor (e.g., A-Button2)
      (gtk-signal-connect grab-key-button "clicked"
			  (lambda ()
			    (gtk-entry-set-text key (car (get-next-event)))))
      
      ;; the bindings list, top
      (gtk-signal-connect 
       clist "select_row" 
       ;; prompt-binding:select-row
       (lambda (row col event)
	 (debug-clist-select "bindings" row col event)
	 (set! current-binding-row row)
	 (let* ((key-cmd-other (gtk-clist-get-row-values clist row 3))
		(key-text (car key-cmd-other))
		(type (cadr key-cmd-other))
		(cmd (caddr key-cmd-other)))
	   (gtk-entry-set-text entry cmd)
	   (gtk-entry-set-text key key-text)
	   (case (string->symbol type)
	     ((Immed Complex)
	      (begin 
		(gtk-widget-hide key-type-option-menu)
		(gtk-widget-show button-type-option-menu)))
	     (else
	      (begin
		(gtk-widget-hide button-type-option-menu))
	      (gtk-widget-show key-type-option-menu)))
	   (set! current-type type))))
      (gtk-widget-show-all vbox)
      (gtk-clist-select-row clist 0 0)
      vbox)))

(define*-public (prompt-binding #&optional (title "Scwm Bindings"))
  (let* ((toplevel (gtk-window-new 'dialog))
	 (vbox (gtk-vbox-new #f 5))
	 (hbox-buttons (gtk-hbutton-box-new))
	 (okbut (gtk-button-new-with-label "Ok"))
;;	 (applybut (gtk-button-new-with-label "Apply"))
	 (cancelbut (gtk-button-new-with-label "Cancel"))
	 (vbox-controls (prompt-binding-vbox)))
    (gtk-box-set-spacing hbox-buttons ui-box-spacing)
    (gtk-container-border-width hbox-buttons ui-box-border)
    (gtk-button-box-set-layout hbox-buttons 'end)
    (gtk-window-set-title toplevel title)
    (gtk-box-pack-start hbox-buttons okbut #f #f)
    (gtk-box-pack-start hbox-buttons cancelbut #f #f)
    (gtk-box-pack-start vbox vbox-controls #t #t)
    (gtk-box-pack-start vbox hbox-buttons #f #f)
    (gtk-container-add toplevel vbox)
    (let ((pp (pointer-position)))
      (gtk-widget-set-uposition toplevel (- (car pp) 150) (cadr pp)))
    (for-each gtk-widget-show (list okbut cancelbut hbox-buttons vbox toplevel))
;;    (gtk-widget-show-all toplevel)
    (gtk-signal-connect okbut "clicked" 
			(lambda () 
			  (gtk-widget-destroy toplevel)))
    (gtk-signal-connect cancelbut "clicked"
			(lambda ()
			  (gtk-widget-destroy toplevel)))
    (lambda ()
      (gtk-widget-hide toplevel)
      (gtk-widget-destroy toplevel))))

;; (prompt-binding)
