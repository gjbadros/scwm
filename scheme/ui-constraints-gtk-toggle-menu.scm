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
  :use-module (app scwm flash-window)
  :use-module (app scwm optargs)
  :use-module (app scwm ui-constraints))

;; (load "/home/jwnichls/scwm/scheme/ui-constraints/toggle-menu.scm")
;; (set-current-module the-root-module)


;; PRIVATE variables held for the module

(define debug-msgwin (make-message-window "Debug Message Window"))

(define gtk-toggle-window (gtk-window-new 'toplevel))
(define gtk-instance-box (gtk-vbutton-box-new))

(define gtk-toggle-close? #f)


;; window flashing code from ui-constraints-toggle-menu

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

(define (make-cn-button n)
  (let* ((toplevel gtk-toggle-window)
	 (box gtk-instance-box)
	 (close? gtk-toggle-close?)
	 (class (ui-constraint-class n))
	 (mproc (ui-constraint-class-menuname-proc class))
	 (name (mproc n))
	 (cn (ui-constraint-cn n))
	 (enabled? (ui-constraint-enabled? n))
	 (but (gtk-button-new-with-label name)))
    (gtk-signal-connect but "clicked" 
			(lambda ()
			  (if (ui-constraint-enabled? n)
			      (disable-ui-constraint n)
			      (enable-ui-constraint n))
			  (if close? (gtk-widget-hide toplevel))))
    (gtk-signal-connect but "enter"
			(lambda ()
			  (map (lambda (c) (flash-windows-of-constraint c)) cn)))
    (gtk-signal-connect but "leave"
			(lambda ()
			  (map (lambda (c) (unflash-windows-of-constraint c)) cn)))
    (gtk-container-add box but)
    (gtk-widget-show but)
    but))



;; sets up the toggle menu for use
;; creates

(define*-public (initialize-gtk-toggle-menu #&optional (close? #f))
  "Creates the GTK resources for the gtk-toggle-menu.
To display the toggle menu, call ui-constraint-gtk-toggle-menu."
  (let* ((toplevel gtk-toggle-window)
	 (vboxen (gtk-vbutton-box-new))
	 (vboxcn gtk-instance-box)
	 (vbox (gtk-vbox-new #f 3))
	 (cn-buttons (map (lambda (n) (make-cn-button n vbox close-window)) 
			  global-constraint-instance-list))
	 (disable (gtk-button-new-with-label "Disable All"))
	 (enable (gtk-button-new-with-label "Enable All"))
	 (close (gtk-button-new-with-label "Close")))
    (add-constraint-add-hook! 
     (lambda (cn) (make-cn-button cn)))
    (set! gtk-toggle-close? close?)
    (gtk-button-box-set-spacing vboxcn 0)
    (gtk-button-box-set-child-ipadding vboxcn 0 0)
    (gtk-button-box-set-child-size vboxcn 300 20)
    (gtk-container-add vbox vboxcn)
    (gtk-widget-show vboxcn)
    (gtk-button-box-set-spacing vboxen 0)
    (gtk-button-box-set-child-ipadding vboxen 0 0)
    (gtk-button-box-set-child-size vboxen 300 20)
    (gtk-container-add vboxen disable)
    (gtk-signal-connect disable "clicked" 
			(lambda () (disable-all-constraints) 
				(if close? (gtk-widget-hide toplevel))))
    (gtk-widget-show disable)
    (gtk-container-add vboxen enable)
    (gtk-signal-connect enable "clicked" 
			(lambda () (enable-all-constraints) 
				(if close? (gtk-widget-hide toplevel))))
    (gtk-widget-show enable)
    (gtk-container-add vbox vboxen)
    (gtk-widget-show vboxen)
    (gtk-container-add vbox close)
    (gtk-signal-connect close "clicked"	(lambda () (gtk-widget-hide toplevel)))
    (gtk-widget-show close)
    (gtk-container-add toplevel vbox)
    (gtk-widget-show vbox)
    (gtk-window-position toplevel 'mouse)
    (gtk-signal-connect toplevel "delete_event" (lambda (args) (gtk-widget-hide toplevel)))
;;    (gtk-widget-show toplevel)
    ))

;; (initialize-gtk-toggle-menu)


(define-public (ui-constraints-gtk-toggle-menu)
  "Displays the GTK version of the constraints toggle menu."
  (gtk-widget-show gtk-toggle-window))

;; (ui-constraints-gtk-toggle-menu)


(define-public (popup-ui-constraints-gtk-toggle-menu)
  "Pops up the constraint toggle menu.
Maintains consistency with previous toggle menu code."
  (ui-constraints-gtk-toggle-menu))

;; (popup-ui-constraints-gtk-toggle-menu)