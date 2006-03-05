;;;; -*-scwm-*-
;;;; $Id$
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
#!
(define guibiff (run-ScwmBiff "mini-mail.xpm" "mini-newmail.xpm"
			      #:action "privtool"))
(guibiff 'go-active)
(guibiff 'go-passive)
(guibiff 'quit)
!#

(define-module (app scwm ScwmBiff)
  :use-module (app scwm base)
  :use-module (app scwm file)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
  :use-module (app scwm optargs))

(define*-public
  (run-ScwmBiff passive-icon-name active-icon-name #:key
		(action noop)
		(parent #f)
		(name "ScwmBiff"))
  (let* ((button (gtk-button-new))
	 (passive-image #f)
	 (active-image #f)
	 (state-active #f)
	 (toplevel (if (not parent)
		       (gtk-window-new 'toplevel)))
	 (imagepath (find-file-in-path passive-icon-name image-load-path))
	 )
    (if (not parent)
	(begin
	  (gtk-window-set-title toplevel name)
	  (gtk-window-set-wmclass toplevel name "Scwm")))
    (if (string? imagepath)
	(begin
	  (set! passive-image (gtk-pixmap-new imagepath button))
	  (gtk-container-add button passive-image))
	(error "Couldn't load passive image."))
    (set! imagepath (find-file-in-path active-icon-name image-load-path))
    (if (string? imagepath)
	(set! active-image (gtk-pixmap-new imagepath button))
	(error "Couldn't load active image."))
    (if (string? action)	;; permit "xterm" to mean (execute "xterm")
	(let ((program-name action))
	  (set! action (lambda () (execute program-name)))))
    (gtk-signal-connect button "clicked" action)
    (if (not parent)
	(begin
	  (gtk-container-add toplevel button)
	  (gtk-widget-show-all toplevel))
	(begin
	  (parent 'add-child button "Mail Notifier")
	  (gtk-widget-show-all button)))
    (lambda (action)
      (case action
	((quit)
	 (if (not (gtk-object-destroyed toplevel))
	     (begin (gtk-widget-unmap toplevel)
		    (gtk-widget-destroy toplevel))))
	((go-active)
	 (beep)
	 (if (not state-active)
	     (begin
	       (gtk-container-remove button passive-image)
	       (gtk-container-add button active-image)
	       (gtk-widget-show-all button)
	       (set! state-active #t))))
	((go-passive)
	 (if state-active
	     (begin
	       (gtk-container-remove button active-image)
	       (gtk-container-add button passive-image)
	       (set! state-active #f))))
	((test)
	 (display active-image) (display passive-image) (newline)))
	)))

(define-public (close-ScwmBiff sb)
  "Close the ScwmBiff notification window."
  (sb 'quit))

(define-public (activate-ScwmBiff sb)
  "Turn on the ScwmBiff notification service."
  (sb 'go-active))

(define-public (deactivate-ScwmBiff sb)
  "Make ScwmBiff no longer notify, but leave visible."
  (sb 'go-passive))
