;; $Id$
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


(define-module (app scwm prompt-bool)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm optargs))



#! args for testing
(use-modules (app scwm gtk)
	     (app scwm optargs)
	     (gtk gtk)
	     (gtk gdk))

(define prompt "Choose value")
(define (proc v) (display v) (newline))
(define title "prompt bool")
(define initval #f)
!#

;;(use-modules (app scwm prompt-bool))
;;(define w (prompt-bool "Do it?" (lambda (v) (display v) (newline)) "Prompt" #t))
(define*-public (prompt-bool prompt proc #&optional
			      (title "prompt-bool")
			      initval)
  (let* ((toplevel (gtk-window-new 'dialog))
	 (vbox (gtk-vbox-new 0 0))
	 (hbox (gtk-hbox-new 0 0))
	 (okbut (gtk-button-new-with-label "Ok"))
	 (cancelbut (gtk-button-new-with-label "Cancel"))
	 (checkbut (gtk-check-button-new-with-label prompt)))
    (gtk-window-set-title toplevel title)
    (gtk-toggle-button-set-state checkbut initval)
    (gtk-box-pack-start vbox checkbut #t #t)
    (gtk-box-pack-start hbox okbut #t #t)
    (gtk-box-pack-start hbox cancelbut #t #t)
    (gtk-box-pack-start vbox hbox #t #t)
    (gtk-container-add toplevel vbox)
    (gtk-widget-show checkbut)
    (gtk-widget-show vbox)
    (gtk-widget-show hbox)
    (gtk-widget-show cancelbut)
    (gtk-widget-show okbut)
    (let ((pp (pointer-position)))
      (gtk-widget-set-uposition toplevel (- (car pp) 150) (cadr pp)))
    (gtk-widget-show toplevel)

    (gtk-signal-connect okbut "pressed" 
			(lambda () 
			  (gtk-widget-destroy toplevel)
			  (proc (string=? "active" (gtk-widget-state checkbut)))))
    (gtk-signal-connect cancelbut "pressed"
			(lambda ()
			  (gtk-widget-destroy toplevel)))
    (lambda ()
      (gtk-widget-hide toplevel)
      (gtk-widget-destroy toplevel))))
