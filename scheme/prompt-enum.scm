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


;;(load "/home/gjb/scwm/scheme/prompt-enum.scm")

(define-module (app scwm prompt-enum)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm prompting-shell)
  :use-module (app scwm optargs))



;;(use-modules (app scwm prompt-enum))
;;(define w (prompt-enum "Focus?" '((click . "Click") (mouse . "Mouse")) (lambda (v) (display "answer = ") (display v) (newline)) #:initval 'mouse))
(define*-public (prompt-enum prompt choices proc #:key
			      (initval #f)
			      (title "prompt-enum"))
  "Prompt with PROMPT for one of CHOICES, and call PROC with result.
CHOICES is an a-list of symbols and strings."
  (let* ((hbox-and-getter (prompt-enum-hbox prompt choices initval))
	 (hbox (car hbox-and-getter))
	 (getter (cadr hbox-and-getter)))
    (prompting-shell proc title hbox getter)))

(define-public (prompt-enum-hbox prompt choices initval)
  "Create and return an enum-prompting hbox and button.
hbox is the gtk container widget, selected-proc?? is a proc
that when invoked returns #t or #f depending on the state
of the boolean displayed in hbox. 
The returned value is a list: (hbox getter).
PROMPT is the prompt to display, CHOICES is an a-list of
symbols and strings. INITVAL is a symbol in the CHOICES a-list.
See also `prompt-enum'." 
  (let* ((hbox (gtk-hbox-new #f 5))
	 (last #f)
	 (choice initval)
	 (label (gtk-label-new prompt)))
    (gtk-box-pack-start hbox label #t #t)
    (let ((radiobuttons 
	   (map (lambda (pair)
		  (let ((but
			 (gtk-radio-button-new-with-label-from-widget last (cdr pair))))
		    ;; (set-object-property! but 'symbol (car pair))
		    (set! last but)
		    (gtk-box-pack-start hbox but #t #t)
		    (if (eq? initval (car pair))
			(begin
			  (gtk-toggle-button-set-state but #t)
			  (set! choice (car pair))))
		    (gtk-signal-connect but "clicked"
					(lambda () (set! choice (car pair))))
		    ))
		choices)))
      (gtk-widget-show-all hbox)
      (list hbox (lambda () choice)))))
