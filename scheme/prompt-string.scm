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



(define-module (app scwm prompt-string)
  :use-module (app scwm gtk)
  :use-module (app scwm file)
  :use-module (gtk gtk)
  :use-module (app scwm optargs))


;; (use-modules (app scwm prompt-string))
;; (use-modules (gtk gtk))
;;(define e (prompt-string "Enter: " (lambda (txt) (display (string-append "Got: " txt "\n"))) #:initval "this is a really long string so see if it grows reasonably"))



(define*-public (prompt-string prompt proc #&key
			       (initval #f) (title "prompt-string"))
  "Use PROMPT as prompt in text entry widget and call PROC with the entered string.
E.g., (prompt-string \"Enter new name\" (lambda (nm) (set-window-title! w nm)))"
  (let* ((toplevel (gtk-window-new 'dialog))
	 (hbox-and-getter-and-entry (prompt-string-hbox prompt initval))
	 (hbox (car hbox-and-getter-and-entry))
	 (getter (cadr hbox-and-getter-and-entry))
	 (entry (caddr hbox-and-getter-and-entry)))
    (gtk-window-set-title toplevel title)
    (gtk-window-set-wmclass toplevel "prompt-string" "Scwm")
    (gtk-container-add toplevel hbox)
    (gtk-signal-connect entry "activate"
			(lambda () (gtk-widget-destroy toplevel) 
				(proc (getter))))
    (gtk-widget-grab-focus entry)
    (let ((pp (pointer-position)))
      (gtk-widget-set-uposition toplevel (- (car pp) 150) (- (cadr pp) 10)))
    (gtk-widget-show-all toplevel)
    (lambda ()
      (gtk-widget-hide toplevel)
      (gtk-widget-destroy toplevel))))


(define-public (prompt-string-hbox prompt initval)
  "Create and return a string-prompting hbox and entry.
PROMPT is the prompt, and INITVAL is the initial string.
The returned value is a list: (hbox getter entry).
See also `prompt-string'."
  (let* ((hbox (gtk-hbox-new #f 0))
	 (entry (gtk-entry-new))
	 (entry-init (if (string? initval) initval ""))
	 (label (gtk-label-new prompt)))
    (gtk-entry-set-text entry entry-init)
    (gtk-box-pack-start hbox label #f #f 10)
    (gtk-box-pack-start hbox entry #t #t)
    (gtk-widget-set-usize entry (min 450 (max 100 (* 8 (string-length entry-init)))) 30)
    (gtk-widget-show-all hbox)
    (list hbox (lambda () (gtk-entry-get-text entry)) entry)))

(define-public (prompt-path-hbox prompt initval)
  "Create and return a path-prompting hbox and entry.
PROMPT is the prompt, and INITVAL is the initial path (a list of strings).
The returned value is a list: (hbox getter entry).
See also `prompt-string'."
  (let* ((answer (prompt-string-hbox prompt (path-list->string-with-colons initval)))
	 (hbox (car answer))
	 (getter (cadr answer))
	 (entry (caddr answer)))
    (list hbox (lambda () (string-with-colons->path-list (getter))) entry)))


(define-public (simple-prompt-proc-hbox prompt initval)
  "Create and return a procedure-prompting hbox and entry.
PROMPT is the prompt, and INITVAL is the initial path (a procedure).
The returned value is a list: (hbox getter entry).
See also `prompt-string'."
  (let* ((procname (procedure-name initval))
	 (answer (prompt-string-hbox 
		  prompt 
		  (if procname 
		      (symbol->string procname) 
		      (procedure-source initval))))
	 (hbox (car answer))
	 (getter (cadr answer))
	 (entry (caddr answer)))
    (list hbox (lambda () (eval (string->symbol (getter)))) entry)))
