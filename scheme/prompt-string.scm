;;;; $Id$
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



(define-module (app scwm prompt-string)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm optargs))


;; (use-modules (app scwm prompt-string))
;; (use-modules (gtk gtk))
;;(define e (prompt-string "Enter: " (lambda (txt) (display (string-append "Got: " txt "\n"))) "a" "this is a really long string so see if it grows reasonably"))



(define*-public (prompt-string prompt proc #&optional (title "prompt-string")
			       initval)
  "Use PROMPT as prompt in text entry widget and call PROC with the entered string.
E.g., (string-prompt \"Enter new name\" (lambda (nm) (set-window-title! w nm)))"
  (let* ((toplevel (gtk-window-new 'dialog))
	 (hbox (gtk-hbox-new #f 0))
	 (entry (gtk-entry-new))
	 (entry-init (if (string? initval) initval ""))
	 (label (gtk-label-new prompt)))
    (gtk-window-set-title toplevel title)
    (gtk-window-set-wmclass toplevel "string-prompt" "Scwm")
    (gtk-container-add toplevel hbox)
    (gtk-entry-set-text entry entry-init)
    (gtk-box-pack-start hbox label #f #f 10)
    (gtk-box-pack-start hbox entry #t #t)
    (gtk-widget-set-usize entry (min 450 (max 100 (* 10 (string-length entry-init)))) 30)
    (gtk-widget-show entry)
    (gtk-widget-show label)
    (gtk-signal-connect entry "activate"
			(lambda () (gtk-widget-destroy toplevel) 
				(proc (gtk-entry-get-text entry))))
    (gtk-widget-show hbox)
    (let ((pp (pointer-position)))
      (gtk-widget-set-uposition toplevel (- (car pp) 150) (cadr pp)))
    (gtk-widget-show toplevel)
    (lambda ()
      (gtk-widget-hide toplevel)
      (gtk-widget-destroy toplevel))))

