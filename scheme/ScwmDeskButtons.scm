;;;; $Id$
;;;; Copyright (C) 1998 Maciej Stachowiak
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



(define-module (app scwm ScwmDeskButtons)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm optargs))



;; MS:FIXME:: Needs a lot of work.

;; MS:FIXME:: My kingdom for MOP!

;; example usage: 
;; (window-style (resource-match?? "ScwmDeskButtons") #:use-style desk-widget)
;;  ;;; or style ScwmDeskButtons windows some other appropriate way.
;; (define desk-buttons 
;;   (run-ScwmDeskButtons 4 #:desk-names '("Work" "Play" "Graphics" "Emacs")))

(define*-public (run-ScwmDeskButtons desks #:key desk-names)
  "Start a ScwmDeskButtons toolbar interface.
DESKS is the number of desktops, and DESK-NAMES can be given to
specify names for each of the desks.  Answers an object that
is used in `close-ScwmDeskButtons' to close the window."
  (let* ((desk-numbers (iota desks))
	 (desk-names (if desk-names
			 desk-names
			 (map number->string desk-numbers)))
	 (desk-buttons (map gtk-button-new-with-label
			    desk-names))
	 (toplevel (gtk-window-new 'toplevel))
	 (hbox (gtk-hbox-new 0 0)))
    (gtk-window-set-title toplevel "ScwmDeskButtons")
    (gtk-window-set-wmclass toplevel "ScwmDeskButtons" "Scwm")
    (gtk-container-add toplevel hbox)
    (for-each (lambda (b) (gtk-box-pack-start hbox b)) desk-buttons)
    (for-each (lambda (b d)
		(gtk-signal-connect b "clicked"
				    (lambda ()
				      (set-current-desk! d))))
	      desk-buttons desk-numbers)
    (gtk-widget-show-all toplevel)
    (lambda ()
      (if (not (gtk-widget-destroyed toplevel))
	  (gtk-widget-hide toplevel)
	  (gtk-widget-destroy toplevel)))))

(define-public (close-ScwmDeskButtons sdb)
  "Close a ScwmDeskButtons object that was returned from `run-ScwmDeskButtons'."
  (sdb))
