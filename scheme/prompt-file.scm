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


(define-module (app scwm prompt-file)
  :use-module (app scwm gtk)
  :use-module (app scwm base)
  :use-module (gtk gtk)
  :use-module (app scwm optargs))



;;(use-modules (app scwm prompt-file))
;;(define w (prompt-file "File to list?" '(0 . 20) (lambda (v) (display v) (newline)) #:initval "Helvetica-12"))
(define*-public (prompt-file prompt proc #:key
			      (initval #f)
			      (title #f)
			      (favorites #f))
  "Prompt using PROMPT for a file and call PROC with value if Ok is clicked.
INITVAL is a default initial file as a string.
TITLE is a window title."
  (let* ((toplevel (gtk-window-new 'dialog))
	 (hbox-and-getter (prompt-file-hbox initval title favorites))
	 (hbox (car hbox-and-getter))
	 (getter (cadr hbox-and-getter))
	 (hbox-buttons (gtk-hbox-new #f 5))
	 (okbut (gtk-button-new-with-label "Ok"))
	 (cancelbut (gtk-button-new-with-label "Cancel")))
    (or title (set! title "prompt-file"))
    (gtk-window-set-title toplevel title)
    (gtk-box-pack-start hbox-buttons okbut #t #t)
    (gtk-box-pack-start hbox-buttons cancelbut #t #t)
    (gtk-box-pack-start hbox hbox-buttons #t #t)
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

(define*-public (prompt-file-hbox prompt initval #:optional favorites)
  "Create and return a file-prompting hbox, complete with link to full file dialog.
PROMPT is the prompt, INITVAL is the initial file as a string.
The returned value is a list: (hbox getter).
See also `prompt-file'."
  (let* ((hbox (gtk-hbox-new #f 0))
	 (cb (if (list? favorites) (gtk-combo-new) #f))
	 (entry (if cb (gtk-combo-entry cb) (gtk-entry-new)))
	 (selbut (gtk-button-new-with-label "Choose..."))
	 (entry-init (or initval ""))
	 (label (gtk-label-new prompt)))
    (if cb
	(gtk-combo-set-popdown-strings cb
				       (list->vector favorites)))
    (gtk-entry-set-text entry entry-init)
    (gtk-box-pack-start hbox label #f #f 10)
    (gtk-box-pack-start hbox (or cb entry) #t #t)
    (gtk-box-pack-start hbox selbut #f #f 10)
    (gtk-widget-set-usize entry (min 450 (max 100 (* 10 (string-length entry-init)))) 30)
    (gtk-widget-show-all hbox)
    (gtk-signal-connect selbut "clicked"
			(lambda ()
			  (let ((dialog (gtk-file-selection-new "File Selection Dialog")))
			    (gtk-signal-connect
			     (gtk-file-selection-ok-button dialog)
			     "clicked" (lambda () 
					 (gtk-entry-set-text 
					  entry 
					  (gtk-file-selection-get-filename dialog))
					 (gtk-widget-destroy dialog)))
			    (gtk-signal-connect
			     (gtk-file-selection-cancel-button dialog)
			     "clicked" (lambda () (gtk-widget-destroy dialog)))
			    (gtk-widget-show dialog))))
    (list hbox (lambda () (gtk-entry-get-text entry)) entry)))
