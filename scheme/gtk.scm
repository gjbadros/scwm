;;;; $Id$
;;;; Copyright (C) 1998-1999 Maciej Stachowiak and Greg J. Badros
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

;; this gets used to protect windows from deletion
;; needs to go in the root module so a gh_lookup
;; will work on it -- gets set below
(define gdk-leader-window #f)


(define-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm optargs) ;; for bound?
  :use-module (gtk gdk)
  :use-module (app scwm scwmgtkhelper))


;; see note above
(if (and (bound? gdk-get-leader-window-id) (procedure? gdk-get-leader-window-id))
    (set! gdk-leader-window (gdk-get-leader-window-id)))

(define-public (scwm-gtk-sync)
  "Dispatch all pending gtk-events.
This ought to be called from inside Scwm-controlled loops that do not return
to the main event loop"
  (if (= 0 (X-server-grabs))
      (while (not (= 0 (gtk-events-pending)))
	     (gtk-main-iteration))))

(restore-scwm-handlers)

(add-input-hook! (fdopen (scwm-gdk-X-fdes) "w+") scwm-gtk-sync)

(define sync-and-add-timer-hook
  (lambda () 
    (scwm-gtk-sync)
    (add-timer-hook! 50000 sync-and-add-timer-hook)))

(sync-and-add-timer-hook) 

(define-public (gtk-pixmap-new-search-scwm-path pixmap-name button)
  "Return the new pixmap object as `gtk-pixmap-new' does, but search Scwm's image-load-path for it."
  (let ((i-l-p image-load-path)
	(answer #f))
    (while (and (not answer) (not (null? i-l-p)))
	   (set! answer (gtk-pixmap-new (string-append (car i-l-p) "/" pixmap-name) button))
	   (set! i-l-p (cdr i-l-p)))
    answer))

;; (define b (gtk-button-new))
;; (gtk-pixmap-new-search-scwm-path "mini-exp-windows-full.xpm" b)
