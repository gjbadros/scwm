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


(define-module (app scwm prompt-bool)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm prompting-shell)
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
;;(define w (prompt-bool "Do it?" (lambda (v) (display v) (newline)) #:initval #t))
(define*-public (prompt-bool prompt proc #:key
			      (initval #f)
			      (title "prompt-bool"))
  "Prompt with PROMPT for a boolean value, and call PROC with result if Ok button is clicked."
  (let* ((hbox-and-getter (prompt-bool-hbox prompt initval))
	 (hbox (car hbox-and-getter))
	 (getter (cadr hbox-and-getter)))
    (prompting-shell proc title hbox getter)))

(define-public (prompt-bool-hbox prompt initval)
  "Create and return a boolean-prompting hbox and button.
hbox is the gtk container widget, selected-proc?? is a proc
that when invoked returns #t or #f depending on the state
of the boolean displayed in hbox.
The returned value is a list: (hbox getter).
See also `prompt-bool'." 
  (let* ((hbox (gtk-hbox-new #f 5))
	 (checkbut (gtk-check-button-new-with-label prompt)))
    (gtk-toggle-button-set-state checkbut initval)
    (gtk-box-pack-start hbox checkbut #t #t)
    (gtk-widget-show-all hbox)
    (list hbox (lambda () (string=? "active" (gtk-widget-state checkbut))))))
