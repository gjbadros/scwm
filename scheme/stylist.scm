;;;; $Id$	-*- scwm -*-
;;;; Copyright (C) 1999 Robert Bihlmeyer
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

(define-module (app scwm stylist)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm winlist)
  :use-module (app scwm wininfo)
  :use-module (app scwm style)
  :use-module (app scwm window-selection)
  :use-module (app scwm flash-window)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm prompt-bool)
  :use-module (app scwm prompt-color))

(define-public *style-list-flash-window-color* (make-color "green"))

(define* (select-style-for predicate #&key (title "Style Selector")
					    (template #f))
  "Select a style for the windows matching PREDICATE.
TITLE is the title of the dialog window; defaults to `Style Selector'.
TEMPLATE is the window where the initial values come from; defaults to a
random window that matches PREDICATE."
  (let* ((windows (list-windows #:only (eval predicate)))
	 (flashed-windows windows)
	 (the-style ()))
    (if (and (not template) (pair? windows))
	(set! template (car windows)))
    (for-each (lambda (w) (flash-window-on w *style-list-flash-window-color*)) windows)
    (let* ((top (gtk-window-new 'toplevel))
	   (tt (gtk-tooltips-new))
	   (apply-hook (make-hook))
	   (Cancel (lambda ignored-args
		     (for-each unflash-window flashed-windows)
		     (flash-selected-windows-on)
		     (gtk-widget-destroy top)))
	   (Apply (lambda ignored-args
		    (for-each unflash-window flashed-windows)
		    (flash-selected-windows-on)
		    (set! flashed-windows ())
		    (run-hook apply-hook)
		    (let ((form `(window-style ,predicate ,@the-style)))
		      (eval form)
		      (write form)
		      (newline))
		    (set! the-style ())))
	   (Ok (lambda args
		 (apply Apply args)
		 (apply Cancel args))))
      (gtk-window-set-title top title)
      (gtk-signal-connect top "delete_event" Cancel)
      (let ((box (gtk-vbox-new #f 5)))
	(gtk-box-pack-start
	 box
	 (let ((table (gtk-table-new 2 15 #f))
	       (row 1))
	   (gtk-table-attach table (gtk-label-new "active") 1 2 0 1 () () 2 5)
	   (for-each
	    (lambda (name prompter getter converter style)
	      (let ((button (gtk-check-button-new)))
		(gtk-table-attach
		 table
		 (let* ((wg (prompter name (getter template)))
			(widget (car wg))
			(getter (cadr wg)))
		   (add-hook! apply-hook
			      (lambda ()
				(if (gtk-toggle-button-active button)
				    (set! the-style
					  (append! the-style
						   (list style (converter
								(getter))))))))
		   widget)
		 0 1 row (1+ row) '(expand fill) '(expand fill) 2 2)
		(gtk-table-attach table button
				  1 2 row (1+ row) '(fill) '(fill) 2 2)
		(set! row (1+ row))))
	    '("Foreground"
	      "Background" "Highlight foreground" "Highlight background"
	      "sticky" "sticky icon" "keep on top"
	      "show title" "squash title" "show resize handles"
	      "show side border"
	      ;"start iconified"
              "start lowered" "start window-shaded")
	    (list prompt-color-hbox prompt-color-hbox
		  prompt-color-hbox prompt-color-hbox
		  prompt-bool-hbox prompt-bool-hbox prompt-bool-hbox
		  prompt-bool-hbox prompt-bool-hbox prompt-bool-hbox
		  prompt-bool-hbox
		  ;prompt-bool-hbox
                  prompt-bool-hbox prompt-bool-hbox)
	    (list (lambda (w) (car (get-window-colors w)))
		  (lambda (w) (unflash-window w)
			  (let ((col (cadr (get-window-colors w))))
			    (flash-window-on w *style-list-flash-window-color*)
			    col))
		  (lambda (w) (or (car (get-window-highlight-colors w))
				  (highlight-foreground)))
		  (lambda (w) (unflash-window w)
			  (or (let ((col (cadr
					  (get-window-highlight-colors w))))
				(flash-window-on w *style-list-flash-window-color*)
				col)
			      (highlight-background)))
		  sticky-window? icon-sticky? kept-on-top?
		  titlebar-shown?
		  (lambda (w) (window-property w 'squashed-titlebar))
		  border-normal?
		  (lambda (w)
		    (not (object-property w 'no-side-decorations)))
		  ;iconified-window?
                  (lambda (w) #f) shaded-window?)
	    (list id id id id
		  id id id
		  not id not
		  not
		  ;id
                  id id)
	    '(#:foreground
	      #:background #:highlight-foreground #:highlight-background
	      #:sticky #:sticky-icon #:kept-on-top
	      #:no-titlebar #:squashed-titlebar #:plain-border
	      #:no-side-decorations
	      ;#:start-iconified ; SRL:FIXME::This style option currently BROKEN.
              #:start-lowered #:start-window-shaded))
	   (gtk-widget-show-all table)
	   table)
	 #t #t)
	(gtk-box-pack-start
	 box
	 (let ((bbox (gtk-hbutton-box-new)))
	   (for-each (lambda (label action)
		       (gtk-box-pack-start
			bbox 
			(let ((button (gtk-button-new-with-label label)))
			  (gtk-signal-connect button "clicked" action)
			  (if (eq? action Cancel)
			      (add-hook! apply-hook
					 (lambda ()
					   (gtk-label-set-text
					    (gtk-button-child button)
					    "Close"))))
			  button)
			#f #f))
		     '("Ok" "Apply" "Cancel")
		     (list Ok Apply Cancel))
	   bbox)
	 #f #f 5)
	(gtk-container-add top box))
      (gtk-widget-show-all top))))

(define*-public (make-window-style-menu #&optional (win (get-window)))
  "Offer a choice to style the window, like named windows, or a whole class.
WIN is the window to style and defaults to the window-context."
  (let ((resource (window-resource win))
	(class (window-class win)))
    (menu
     (list
      (menu-title "Style")
      menu-separator
      (menuitem "&this &window"
		#:action (lambda () (select-style-for
				     `(lambda (w) (eq? w ,win))
				     #:template win
				     #:title (string-append "Style `"
							    (window-title win)
							    "'"))))
      (menuitem (string-append "windows &named `" resource "'")
		#:action (lambda () (select-style-for
				     `(resource-match?? ,resource)
				     #:template win
				     #:title (string-append "Style `" resource
							    "' windows"))))
      (menuitem (string-append "windows of &class `" class "'")
		#:action (lambda () (select-style-for
				     `(class-match?? ,class)
				     #:template win
				     #:title (string-append "Style `" class
							    "' class"))))))))
#! for testing
(bind-mouse 'left-button-1 3 popup-style-menu)
!#

#! Styles to do:

#:border-width
#:focus
#:icon-title
#:icon-box
#:mwm-buttons
#:mwm-border
#:show-icon
#:force-icon
#:icon
#:mini-icon
#:button
#:no-button
#:hint-override
#:decorate-transient
#:mwm-decor-hint
#:mwm-func-hint
#:PPosition-hint
#:OL-decor-hint
#:start-on-desk
#:skip-mapping
#:lenience
#:placement-proc
#:transient-placement-proc
#:use-theme

auto-raise
#:auto-raise
#:auto-raise-delay
#:auto-raise-unfocus-delay
#:auto-raise-focus-proc
#:auto-raise-unfocus-proc

decor
#:use-decor

hover-focus
#:hover-focus-delay
#:hover-focus

winlist
#:winlist-skip
#:circulate-skip
#:circulate-skip-icon

winops
#:start-maximized

#:other-proc
#:other-hint-proc

!#
