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



(define-module (app scwm ScwmClock)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm time-convert)
  :use-module (app scwm optargs))



(define (date-string format)
  (strftime format (localtime (current-time))))

(define*-public (run-ScwmClock #&key (24-hour #t) (show-timezone #t)
			       (update-interval 1))
  (let* ((time-format (string-append (if 24-hour "%T" "%r")
				     (if show-timezone " %Z" "")))
	 (update-usec (* 1000000 update-interval))
	 (toplevel (gtk-window-new 'toplevel))
	 (label (gtk-label-new (date-string time-format))))
    (gtk-window-set-title toplevel "ScwmClock")
    (gtk-window-set-wmclass toplevel "ScwmClock" "Scwm")
    (gtk-container-add toplevel label)
    (gtk-widget-show label)
    (letrec ((handle #f)
	     (update-string-and-add-timer 
	      (lambda ()
		(gtk-label-set label (date-string time-format))
		(set! handle (add-timer-hook! 1000000
					      update-string-and-add-timer))))
	     (remove-hook (lambda () 
			    (if handle (remove-timer-hook! handle)))))
      (update-string-and-add-timer)
      (gtk-widget-show toplevel)
      (lambda () 
	(if (not (gtk-object-destroyed toplevel))
	    (gtk-widget-unmap toplevel)
	    (gtk-widget-destroy toplevel))
	(remove-hook)))))

(define-public (close-ScwmClock sc)
  (sc))



