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


(define-module (app scwm prompt-range)
  :use-module (app scwm gtk)
  :use-module (app scwm base)
  :use-module (gtk gtk)
  :use-module (app scwm prompting-shell)
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
;;(define w (prompt-range "Value?" '(0 . 20) (lambda (v) (display v) (newline)) #:initval 3))
;;(define w (prompt-integer-range "Value?" '(0 . 20) (lambda (v) (display v) (newline)) #:initval 1))
(define*-public (prompt-range prompt range proc #:key
			      (initval #f)
			      (title #f)
			      (digits 1))
  "Prompt using PROMPT for a numeric value in RANGE, and call PROC with value if Ok is clicked.
RANGE is a cons cell (low . hight); ranges are inclusive. 
INITVAL is a default initial value.
TITLE is a window title."
  (let* ((hbox-and-getter (prompt-range-hbox prompt range initval digits))
	 (hbox (car hbox-and-getter))
	 (getter (cadr hbox-and-getter)))
    (prompting-shell proc title hbox getter)))

(define*-public (prompt-integer-range prompt range proc #:key
				      (initval #f) (title #f))
  "Prompt using PROMPT for an integer value in RANGE, and call PROC with value if Ok is clicked.
RANGE is a cons cell (low . high); ranges are inclusive. 
INITVAL is a default initial value.
TITLE is a window title."
  (prompt-range prompt range (lambda (v) (proc (inexact->exact v)))
		#:initval initval
		#:title title
		#:digits 0))



(define*-public (prompt-range-hbox prompt range initval #:optional (digits 1))
  "Create and return a range-prompting hbox and scale, label.
PROMPT is the prompt, RANGE is the allowed rane, and INITVAL is the initial string.
The returned value is a list: (hbox getter).
See also `prompt-range'."
  (let* ((hbox (gtk-hbox-new #f 5))
	 (label (gtk-label-new prompt))
	 (delta (- (cdr range) (car range)))
	 (page-inc (scwm-round/ delta 10))
	 ;; gtk-adjustment-new value lower upper step-inc page-inc page-size
	 (adjustment (gtk-adjustment-new (or initval (car range))
					 (car range) (cdr range)
					 1.0 page-inc 0.0))
	 (scale (gtk-hscale-new adjustment)))
    ;; GJB:FIXME:: these numbers are lame-- should be based on 
    ;; the width of the parent and the height of the font
    ;;(gtk-widget-set-usize scale 500 30)
    (gtk-range-set-update-policy scale 'delayed)
    (gtk-scale-set-digits scale digits)
    (gtk-scale-set-draw-value scale #t)
    (gtk-box-pack-start hbox label #f #f 5)
    (gtk-box-pack-start hbox scale #t #t)
    (gtk-widget-show-all hbox)
    (list hbox (lambda () (gtk-adjustment-value adjustment)))))

(define-public (prompt-integer-range-hbox prompt range initval)
  "Create and return an integer-range-prompting hbox and scale, label.
PROMPT is the prompt, RANGE is the allowed rane, and INITVAL is the initial string.
The returned value is a list: (hbox getter).
See also `prompt-range', `prompt-integer-range', `prompt-range-hbox'."
  (let* ((answer (prompt-range-hbox prompt range initval 0))
	 (hbox (car answer))
	 (getter (cadr answer)))
    (list hbox (lambda () (inexact->exact (getter))))))

