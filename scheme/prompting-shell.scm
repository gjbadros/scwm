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


(define-module (app scwm prompting-shell)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm optargs))



(define*-public (prompting-shell proc title hbox getter)
  "Create a GTk+ shell window and return a procedure that closes it.
PROC is the procedure to run when the okay button is clicked, and
it is called with a single argument, the result of applying
GETTER to no arguments.  TITLE is the title of the window,
and HBOX is a Gtk HBox widget that the shell should contain."
  (let* ((toplevel (gtk-window-new 'dialog))
	 (vbox (gtk-vbox-new #f 5))
	 (hbox-buttons (gtk-hbox-new #f 5))
	 (okbut (gtk-button-new-with-label "Ok"))
	 (cancelbut (gtk-button-new-with-label "Cancel")))
    (gtk-window-set-title toplevel title)
    (gtk-box-pack-start hbox-buttons okbut #t #t)
    (gtk-box-pack-start hbox-buttons cancelbut #t #t)
    (gtk-box-pack-start vbox hbox #t #t)
    (gtk-box-pack-start vbox hbox-buttons #t #t)
    (gtk-container-add toplevel vbox)
    (let ((pp (pointer-position)))
      (gtk-widget-set-uposition toplevel (- (car pp) 150) (cadr pp)))
    (gtk-widget-show-all toplevel)
    (gtk-signal-connect okbut "clicked" 
			(lambda () 
			  (gtk-widget-destroy toplevel)
			  (proc (getter))))
    (gtk-signal-connect cancelbut "clicked"
			(lambda ()
			  (gtk-widget-destroy toplevel)))
    (lambda ()
      (gtk-widget-hide toplevel)
      (gtk-widget-destroy toplevel))))
