;;;; $Id$ -*-scwm-*-
;;;; Copyright (C) 1999 Glenn Trigg
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

(define*-public (run-ScwmClock #:key (24-hour #t) (show-timezone #t)
			       (update-interval 1) (parent #f))
  "Start a ScwmClock window display.
24-HOUR is #t if you want to use military time, #f for am/pm display.
SHOW-TIMEZONE is #t to display the timezeon.
UPDATE-INTERVAL is how often (in seconds) that the clock should be updated.
PARENT is a parent window to use for the clock."
  (let* ((time-format (string-append (if 24-hour "%T" "%r")
				     (if show-timezone " %Z" "")))
	 (update-msec (sec->msec update-interval))
	 (frame (gtk-frame-new #f))
	 (label (gtk-label-new (date-string time-format))))
    (if (not parent)
	(begin (define toplevel (gtk-window-new 'toplevel))
	       (gtk-window-set-title toplevel "ScwmClock")
	       (gtk-window-set-wmclass toplevel "ScwmClock" "Scwm")
	       (gtk-container-add toplevel frame)
	       (gtk-widget-show toplevel))
	(parent 'add-child frame "Current Time"))
    (gtk-container-add frame label)
    (gtk-frame-set-shadow-type frame 'etched-out)
    (gtk-widget-show label)
    (gtk-widget-show frame)
    (letrec ((handle #f)
	     (update-string-and-add-timer 
	      (lambda ()
		(gtk-label-set-text label (date-string time-format))
		(set! handle (add-timer-hook! update-msec
					      update-string-and-add-timer))))
	     (remove-hook (lambda () 
			    (if handle (remove-timer-hook! handle)))))
      (update-string-and-add-timer)
      (lambda ()
	(if (not parent)
	    (if (not (gtk-object-destroyed toplevel))
		(gtk-widget-unmap toplevel)
		(gtk-widget-destroy toplevel))
	    (if (not (gtk-object-destroyed label))
		(gtk-widget-unmap label)
		(gtk-widget-destroy label)))
	(remove-hook)))))

(define-public (close-ScwmClock sc)
  "Close the ScwmClock with handle SC."
  (sc))

#!
(define clock (run-ScwmClock #:show-timezone #f #:update-interval 5))
!#
