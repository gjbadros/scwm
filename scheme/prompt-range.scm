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


(define-module (app scwm prompt-range)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm optargs))



#! args for testing
(use-modules (app scwm gtk)
	     (app scwm optargs)
	     (gtk gtk)
	     (gtk gdk))

(define prompt "Choose value")
(define range '(0 . 10))
(define (proc v) (display v))
(define title "prompt range")
(define initval #f)
!#

;;(use-modules (app scwm prompt-range))
;;(define w (prompt-range "Value?" '(0 . 20) (lambda (v) (display v) (newline)) "Prompt" 3))
;;(define w (prompt-integer-range "Value?" '(0 . 20) (lambda (v) (display v) (newline)) "Prompt" 3))
(define*-public (prompt-range prompt range proc #&optional
			      (title "prompt-range")
			      initval)
  (let* ((toplevel (gtk-window-new 'dialog))
	 (vbox (gtk-vbox-new 0 0))
	 (hbox (gtk-hbox-new 0 0))
	 (okbut (gtk-button-new-with-label "Ok"))
	 (cancelbut (gtk-button-new-with-label "Cancel"))
	 (label (gtk-label-new prompt))
	 ;; gtk-adjustment-new value lower upper step-inc page-inc page-size
	 (adjustment (gtk-adjustment-new (or initval (car range))
					 (car range) (cdr range)
					 1.0 5.0 5.0))

	 (scale (gtk-hscale-new adjustment)))
    (gtk-window-set-title toplevel title)
    (gtk-widget-set-usize scale 150 30)
    (gtk-range-set-update-policy scale 'delayed)
    (gtk-scale-set-digits scale (+ 1 (log10 (cdr range))))
    (gtk-scale-set-draw-value scale #t)
    (gtk-box-pack-start vbox label #t #t)
    (gtk-box-pack-start vbox scale #t #t)
    (gtk-box-pack-start hbox okbut #t #t)
    (gtk-box-pack-start hbox cancelbut #t #t)
    (gtk-box-pack-start vbox hbox #t #t)
    (gtk-container-add toplevel vbox)
    (gtk-widget-show label)
    (gtk-widget-show scale)
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
			  (proc (gtk-adjustment-value adjustment))))
    (gtk-signal-connect cancelbut "pressed"
			(lambda ()
			  (gtk-widget-destroy toplevel)))
    (lambda ()
      (gtk-widget-hide toplevel)
      (gtk-widget-destroy toplevel))))

(define*-public (prompt-integer-range prompt range proc #&optional
				      title initval)
  (prompt-range prompt range (lambda (v) (proc (inexact->exact v))) title initval))
