;;;; $Id$
;;;; Copyright (C) 1999 Jeff W. Nichols
;;;; Based on ui-constraints-buttons.scm, Copyright (C) 1999 Greg J. Badros
;;;; Some elements copied from ui-constraints-toggle-menu.scm,
;;;;                                 also Copyright (C) 1999 Greg J. Badros
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

;; toggle-menu.scm
;;
;; My attempt to make a better constraint menu using GTK+.
;; Ideally, this menu should provide obvious methods for
;; setting the enable of a constraint, deletion of a 
;; constraint, and especially some easy way to organize
;; constraints.

(define-module (app scwm ui-constraints-gtk-toggle-menu)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
;;  :use-module (app scwm flash-window)
  :use-module (app scwm ui-constraints))

;; (load "/home/jwnichls/scwm/scheme/ui-constraints/toggle-menu.scm")
;; (set-current-module the-root-module)

(define (flash-windows-of-constraint cn)
  (let ((color "red"))
    (for-each 
     (lambda (w) (flash-window w #:color color #:unflash-delay #f)) 
     (cl-windows-of-constraint cn))))

(define (unflash-windows-of-constraint cn)
  (for-each 
   (lambda (w) (unflash-window w))
   (cl-windows-of-constraint cn)))

;; make the button for a particular constraint
;; instance -- PRIVATE

(define (make-cn-button n box)
  (let* ((class (ui-constraint-class n))
	 (mproc (ui-constraint-class-menuname-proc class))
	 (name (mproc n))
	 (cn (ui-constraint-cn n))
	 (enabled? (ui-constraint-enabled? n))
	 (but (gtk-button-new-with-label name)))
    (gtk-signal-connect but "clicked" 
			(lambda ()
			  (if enabled?
			      (disable-ui-constraint n)
			      (enable-ui-constraint n))
			  (gtk-widget-hide toplevel)))
    (gtk-signal-connect but "enter"
			(lambda ()
			  (map (lambda (c) (flash-windows-of-constraint c)) cn)))
    (gtk-signal-connect but "leave"
			(lambda ()
			  (map (lambda (c) (unflash-windows-of-constraint c)) cn)))
    (gtk-container-add box but)
    (gtk-widget-show but)
    but))

;; displays the toggle menu
;; to use, modify constraints.scm to bind
;; ui-constraints-gtk-toggle-menu to "C-M-S-c"

(define-public (ui-constraints-gtk-toggle-menu)
  "Displays the GTK+ variant of the constraint instance menu.
Allows the user to disable/enable constraints on their windows."
  (let* ((toplevel (gtk-window-new 'popup))
	 (vbox (gtk-vbutton-box-new))
	 (cn-buttons (map (lambda (n) (make-cn-button n vbox)) global-constraint-instance-list))
	 (disable (gtk-button-new-with-label "Disable All"))
	 (enable (gtk-button-new-with-label "Enable All"))
	 (close (gtk-button-new-with-label "Close")))
    (gtk-button-box-set-spacing vbox 0)
    (gtk-button-box-set-child-ipadding vbox 0 0)
    (gtk-button-box-set-child-size vbox 300 20)
    (gtk-container-add vbox disable)
    (gtk-signal-connect disable "clicked" 
			(lambda () (disable-all-constraints) (gtk-widget-hide toplevel)))
    (gtk-widget-show disable)
    (gtk-container-add vbox enable)
    (gtk-signal-connect enable "clicked" 
			(lambda () (enable-all-constraints) (gtk-widget-hide toplevel)))
    (gtk-widget-show enable)
    (gtk-container-add vbox close)
    (gtk-signal-connect close "clicked"	(lambda () (gtk-widget-hide toplevel)))
    (gtk-widget-show close)
    (gtk-container-add toplevel vbox)
    (gtk-widget-show vbox)
    (gtk-window-position toplevel 'mouse)
    (gtk-signal-connect toplevel "delete_event" (lambda (args) (gtk-widget-hide toplevel)))
    (gtk-widget-show toplevel)))

;; (ui-constraints-new-toggle-menu)

(define-public (popup-ui-constraints-gtk-toggle-menu)
  "Pops up the constraint toggle menu.
Maintains consistency with previous toggle menu code."
  (ui-constraints-gtk-toggle-menu))






    
