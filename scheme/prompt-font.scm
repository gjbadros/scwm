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


(define-module (app scwm prompt-font)
  :use-module (app scwm gtk)
  :use-module (app scwm base)
  :use-module (gtk gtk)
  :use-module (app scwm optargs))



#! args for testing
(use-modules (app scwm gtk)
	     (app scwm optargs)
	     (gtk gtk)
	     (gtk gdk))

(define prompt "Choose value")
(define (proc v) (display v))
(define title "prompt font")
(define initval "Helvetica-12")
!#

;;(use-modules (app scwm prompt-font))
;;(define w (prompt-font "Window font?" '(0 . 20) (lambda (v) (display v) (newline)) #:initval "Helvetica-12"))
(define*-public (prompt-font prompt proc #&key
			      (initval #f)
			      (title #f))
  "Prompt using PROMPT for a font and call PROC with value if Ok is clicked.
INITVAL is a default initial font as a font object or string.
TITLE is a window title."
  (let* ((hbox-and-getter (prompt-font-hbox prompt initval))
	 (hbox (car hbox-and-getter))
	 (getter (cadr hbox-and-getter)))
    (prompting-shell proc title hbox getter)))

(define*-public (prompt-font-hbox prompt initval)
  "Create and return a font-prompting hbox, complete with link to full font dialog.
PROMPT is the prompt, INITVAL is the initial font as a font object or a string.
The returned value is a list: (hbox getter).
See also `prompt-font'."
  (let* ((hbox (gtk-hbox-new #f 0))
	 (entry (gtk-entry-new))
	 (initvalfont (cond
		       ((font? initval) initval)
		       ((string? initval) (make-font initval))
		       (else 
			(make-font "fixed"))))
	 (selbut (gtk-button-new-with-label "Pick..."))
	 (entry-init (font-property initvalfont 'name))
	 (label (gtk-label-new prompt)))
    (gtk-entry-set-text entry entry-init)
    (gtk-box-pack-start hbox label #f #f 10)
    (gtk-box-pack-start hbox entry #t #t)
    (gtk-box-pack-start hbox selbut #f #f 10)
    (gtk-widget-set-usize entry (min 450 (max 100 (* 10 (string-length entry-init)))) 30)
    (gtk-widget-show-all hbox)
    (gtk-signal-connect selbut "clicked"
			(lambda ()
			  (let ((dialog (gtk-font-selection-dialog-new "Font Selection Dialog")))
			    (gtk-signal-connect
			     (gtk-font-selection-dialog-ok-button dialog)
			     "clicked" (lambda () 
					 (gtk-entry-set-text 
					  entry 
					  (gtk-font-selection-dialog-get-font-name dialog))
					 (gtk-widget-destroy dialog)))
			    (gtk-font-selection-dialog-set-preview-text dialog
			     "1234567890 abcdefg !@#$%^&*(){}[]\\|=+_/?: Scwm rocks!")
			    (gtk-signal-connect
			     (gtk-font-selection-dialog-cancel-button dialog)
			     "clicked" (lambda () (gtk-widget-destroy dialog)))
			    (gtk-widget-show dialog))))
    (list hbox (lambda () (make-font (gtk-entry-get-text entry))) entry)))
