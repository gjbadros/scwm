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



(define-module (app scwm prompt-binding)
  :use-module (ice-9 session)
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


(define (populate-clist-with-bindings-from clist context)
  (let ((x #("Key" "Type" "Command")))
    (for-each
     (lambda (binding)
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
		     (vector-set! x 1 "")
		     (vector-set! x 2 procname)
		     (gtk-clist-append clist x)))
	       (if proc2
		   (begin
		     (vector-set! x 1 "Immed")
		     (vector-set! x 2 procname2)
		     (gtk-clist-append clist x))))
	     ;; not a mouse binding (i.e., key)
	     (begin
	       (if proc
		   (begin
		     (vector-set! x 1 "Press")
		     (vector-set! x 2 procname)
		     (gtk-clist-append clist x)))
	       (if proc2
		   (begin
		     (vector-set! x 1 "Release")
		     (vector-set! x 2 procname2)
		     (gtk-clist-append clist x)))))))
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


(define-public (prompt-binding-vbox)
  (let* ((name (symbol->string (caar contexts-and-descriptions)))
	 (description (string-append descr-prefix-string (cdar contexts-and-descriptions)))
	 (context-frame (gtk-frame-new name))
	 (vbox (gtk-vbox-new #f 0))
	 (ins-del-bbox (gtk-hbutton-box-new))
	 (vbox-2 (gtk-vbox-new #f 0))
	 (context-label (gtk-label-new description))
	 (insert (gtk-button-new-with-label "Insert"))
	 (change-ev (gtk-button-new-with-label "Change Event"))
	 (change-pr (gtk-button-new-with-label "Change Proc"))
	 (delete (gtk-button-new-with-label "Delete"))
	 (clist (gtk-clist-new-with-titles #("Key" "Type" "Command")))
	 (scroller (gtk-scrolled-window-new)))
    
    (gtk-widget-set-state insert 'insensitive)
    (gtk-widget-set-state change-ev 'insensitive)
    (gtk-widget-set-state change-pr 'insensitive)
    (gtk-widget-set-state delete 'insensitive)

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
    (gtk-container-add ins-del-bbox change-ev)
    (gtk-container-add ins-del-bbox change-pr)
    (gtk-container-add ins-del-bbox delete)
    (populate-clist-with-bindings-from clist (caar contexts-and-descriptions))

    (gtk-box-pack-start vbox-2 context-label)
    (gtk-label-set-justify context-label 'left)
    (gtk-label-set-line-wrap context-label #t)
    (gtk-container-add vbox-2 vbox)
    (gtk-container-add context-frame vbox-2)
    (let*
	((hbox-1 (gtk-hbox-new #f 0))
	 (vbox-2 (gtk-vbox-new #f 0))
	 (vbox (gtk-vbox-new #f 0))
	 (entry (gtk-entry-new))
	 (map-clist (gtk-clist-new-with-titles #("Context")))
	 (doc-frame (gtk-frame-new "Procedure Documentation"))
	 (cmd-clist (gtk-clist-new-with-titles #("Commands" "Module")))
	 (scroller (gtk-scrolled-window-new))
	 (scroller-2 (gtk-scrolled-window-new))
	 (doc-textbox (gtk-text-new #f #f))
	 (scroller-doc (gtk-scrolled-window-new)))
;;      (set! pb-cmd-clist cmd-clist) ;; public for debugging
      ;;    (gtk-box-pack-start doc-frame scroller-doc)
      (gtk-box-set-spacing hbox-1 ui-box-spacing)
      (gtk-container-border-width hbox-1 ui-box-border)
      (gtk-box-set-spacing vbox-2 ui-box-spacing)
      (gtk-container-border-width vbox-2 ui-box-border)
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

      (gtk-container-add vbox-2 entry)
      (gtk-container-add vbox-2 scroller)

      ;; 2. the context selection widget
      (gtk-scrolled-window-set-policy scroller-2 'automatic 'automatic)
      (gtk-clist-set-column-auto-resize map-clist 0 #t)
      (gtk-clist-set-selection-mode map-clist 'browse)
      (gtk-widget-set-usize scroller-2 80 100)
      (gtk-widget-set-usize scroller-doc 100 50)
      (gtk-container-add scroller-2 map-clist)
      (gtk-container-add hbox-1 scroller-2)
      (gtk-container-add scroller-doc doc-textbox)
      (gtk-container-add hbox-1 vbox-2)
      
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
			      (gtk-frame-set-label context-frame 
						   (symbol->string (car sym-and-descr)))
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
		(docs (and m-p (proc-doc (procedure-string->procedure (car m-p))))))
	   (gtk-text-replace doc-textbox (if (string? docs) docs "")))
	 ))

      ;; the text entry just about the command list
      (gtk-signal-connect 
       entry "changed"
       ;; prompt-binding:set-event
       (lambda ()
	 (let* ((cmd (gtk-entry-get-text entry))
		(cmd-row (clist-find cmd-clist 2
				     (lambda (ccmd mod)
				       (string-ci-has-prefix ccmd cmd)))))
	   (if (>= cmd-row 0)
	       (begin
		 (gtk-clist-select-row cmd-clist cmd-row 0)
		 (gtk-clist-moveto cmd-clist cmd-row 0 .5 .5)))
	   )))

      ;; the insert button in the binding frame at the top
      (gtk-signal-connect insert "clicked" 
			  ;; prompt-binding:insert
			  (lambda ()
			    noop))

      ;; the delete button in the binding frame at the top
      (gtk-signal-connect change-ev "clicked" 
			  ;; prompt-binding:change-ev
			  (lambda ()
			    noop))

      ;; the delete button in the binding frame at the top
      (gtk-signal-connect change-pr "clicked" 
			  ;; prompt-binding:change-pr
			  (lambda ()
			    noop))

      ;; the delete button in the binding frame at the top
      (gtk-signal-connect delete "clicked" 
			  ;; prompt-binding:delete
			  (lambda ()
			    noop))
      
      ;; the bindings list, top
      (gtk-signal-connect 
       clist "select_row" 
       ;; prompt-binding:select-row
       (lambda (row col event)
	 (debug-clist-select "bindings" row col event)
	 (let* ((key-cmd-other (gtk-clist-get-row-values clist row 3))
		(cmd (caddr key-cmd-other)))
	   (gtk-entry-set-text entry cmd)
	   )))
       
      vbox)))

(define*-public (prompt-binding #&optional (title "Bindings"))
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
    (gtk-widget-show-all toplevel)
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
