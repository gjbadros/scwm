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



(define-module (app scwm string-prompt)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm optargs))


;; (use-modules (app scwm string-prompt))
;;(define e (string-prompt "Enter: " (lambda (txt) (display (string-append "Got: " txt "\n")))))



(define-public (string-prompt prompt proc)
  "Use PROMPT as prompt in text entry widget and call PROC with the entered string.
E.g., (string-prompt \"Enter new name\" (lambda (nm) (set-window-title! w nm)))"
  (let* ((toplevel (gtk-window-new 'toplevel))
	 (hbox (gtk-hbox-new 0 0))
	 (entry (gtk-entry-new))
	 (label (gtk-label-new prompt)))
    (gtk-window-set-title toplevel "string-prompt")
    (gtk-window-set-wmclass toplevel "string-prompt" "Scwm")
    (gtk-container-add toplevel hbox)
    (gtk-widget-show entry)
    (gtk-widget-show label)
    (gtk-box-pack-start hbox label)
    (gtk-box-pack-start hbox entry)
    (gtk-signal-connect entry "activate"
			(lambda () (gtk-widget-destroy toplevel) 
				(proc-to-call-with-label (gtk-entry-get-text entry))))
    (gtk-widget-show hbox)
    (gtk-widget-show toplevel)
    (lambda ()
      (if (not (gtk-widget-destroyed toplevel))
	  (gtk-widget-hide toplevel)
	  (gtk-widget-destroy toplevel)))))

