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

(use-modules (app scwm flash-window)
	     (app scwm gtk)
	     (gtk gtk)
	     (app scwm prompt-color)
	     (app scwm prompt-bool))

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
    (for-each flash-window-on windows)
    (let* ((top (gtk-window-new 'toplevel))
	   (tt (gtk-tooltips-new))
	   (apply-hook ())
	   (Cancel (lambda ignored-args
		     (for-each unflash-window flashed-windows)
		     (gtk-widget-destroy top)))
	   (Apply (lambda ignored-args
		    (for-each unflash-window flashed-windows)
		    (set! flashed-windows ())
		    (run-hooks apply-hook)
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
	(for-each
	 (lambda (name getter style)
	   (gtk-box-pack-start box
			       (let* ((wg (prompt-color-hbox name (getter
								   template)))
				     (widget (car wg))
				     (getter (cadr wg)))
				 (add-hook!
				  apply-hook
				  (lambda ()
				    (set! the-style
					  (append! the-style
						   (list style (getter))))))
				 (gtk-box-pack-start
				  widget
				  (let ((button (gtk-check-button-new)))
				    (gtk-widget-show button)
				    button)
				  #f #f)
				 widget)
			       #f #f))
	 '("Foreground"
	   "Background" "Highlight foreground" "Highlight background")
	 (list (lambda (w) (car (get-window-colors w)))
	       (lambda (w) (unflash-window w)
		       (let ((col (cadr (get-window-colors w))))
			 (flash-window-on w)
			 col))
	       (lambda (w) (or (car (get-window-highlight-colors w))
			       (highlight-foreground)))
	       (lambda (w) (unflash-window w)
		       (or (let ((col (cadr (get-window-highlight-colors w))))
			     (flash-window-on w)
			     col)
			   (highlight-background))))
	 '(#:foreground
	   #:background #:highlight-foreground #:highlight-background))
	(for-each
	 (lambda (name getter style)
	   (gtk-box-pack-start
	    box
	    (let* ((wg (prompt-bool-hbox name (getter template)))
		   (widget (car wg))
		   (getter (cadr wg)))
	      (add-hook! apply-hook
			 (lambda ()
			   (set! the-style
				 (append! the-style
					  (if (pair? style)
					      (list (car style) (not (getter)))
					      (list style (getter)))))))
	      widget)
	    #f #f))
	 '("sticky" "sticky icon" "keep on top"
		    "show title" "squash title" "show resize handles"
		    "show side border"
		    "start iconified" "start lowered" "start window-shaded")
	 (list sticky? icon-sticky? kept-on-top?
	       titlebar-shown?
	       (lambda (w) (window-property w 'squashed-titlebar))
	       border-normal?
	       (lambda (w) (not (object-property w 'no-side-decorations)))
	       iconified? (lambda (w) #f) window-shaded?)
	 '(#:sticky #:sticky-icon #:kept-on-top
		    (#:no-titlebar) #:squashed-titlebar (#:plain-border)
		    (#:no-side-decorations)
		    #:start-iconified #:start-lowered #:start-window-shaded))
	(gtk-box-pack-start
	 box
	 (let ((bbox (gtk-hbox-new #f 0)))
	   (for-each (lambda (label action)
		       (gtk-box-pack-start
			bbox 
			(let ((button (gtk-button-new-with-label label)))
			  (gtk-signal-connect button "pressed" action)
			  (gtk-widget-show button)
			  (if (eq? action Cancel)
			      (add-hook! apply-hook
					 (lambda ()
					   (gtk-label-set
					    (gtk-button-child button)
					    "Close"))))
			  button)
			#t #f))
		     '("Ok" "Apply" "Cancel")
		     (list Ok Apply Cancel))
	   (gtk-widget-show bbox)
	   bbox)
	 #f #f)
	(gtk-container-add top box)
	(gtk-widget-show box))
      (gtk-widget-show top))))

(define* (popup-style-menu #&optional (win (get-window)))
  "Offer a choice to style the window, like named windows, or a whole class.
WIN is the window to style and defaults to the window-context."
  (let ((resource (window-resource win))
	(class (window-class win)))
    (popup-menu
     (menu
      (list
       (menu-title "Style ...")
       (menuitem "... &this &window"
		 #:action (lambda () (select-style-for
				      `(lambda (w) (eq? w ,win))
				      #:template win
				      #:title (string-append "Style `"
							     (window-title win)
							     "'"))))
       (menuitem (string-append "... windows &named `" resource "'")
		 #:action (lambda () (select-style-for
				      `(resource-match?? ,resource)
				      #:template win
				      #:title (string-append "Style `" resource
							     "' windows"))))
       (menuitem (string-append "... windows of &class `" class "'")
		 #:action (lambda () (select-style-for
				      `(class-match?? ,class)
				      #:template win
				      #:title (string-append "Style `" class
							     "' class")))))))))
#! for testing
(bind-mouse 'button-1 3 popup-style-menu)
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
#:random-placement
#:smart-placement
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