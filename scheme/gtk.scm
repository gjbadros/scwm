;;;; $Id$
;;;; Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
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
  :use-module (gtk gdk)
  :use-module (app scwm optargs) ;; for bound?
  :use-module (app scwm base)
  :use-module (app scwm file) ;; for find-file-in-path
  :use-module (app scwm scwmgtkhelper))


(restore-scwm-handlers)

(define-public scwm-gtk-timer-hook-enabled? #t)

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

(define-public sync-and-add-timer-hook
  (lambda () 
    (scwm-gtk-sync)
    (if scwm-gtk-timer-hook-enabled?
	(add-timer-hook! 150 sync-and-add-timer-hook))))

(define-public (start-handling-gtk-events)
  (add-input-hook! (fdopen (scwm-gdk-X-fdes) "w+") scwm-gtk-sync)
  (sync-and-add-timer-hook))

(define-public (gtk-pixmap-new-search-scwm-path pixmap-name button)
  "Return the new pixmap object as `gtk-pixmap-new' does, but search Scwm's image-load-path for it."
  (let ((imagefile (find-file-in-path pixmap-name image-load-path)))
    (if imagefile (gtk-pixmap-new imagefile button) #f)))

(define-public (image->gtk-pixmap img button)
  "Return a gtk-pixmap widget for IMG for use in BUTTON."
  (if (string? img)
      (gtk-pixmap-new-search-scwm-path img button)
      (gtk-pixmap-new (image-property img 'filename) button)))

;; (define b (gtk-button-new))
;; (gtk-pixmap-new-search-scwm-path "mini-exp-windows-full.xpm" b)


(define-public (gtk-window->scwm-window gtkwin)
  "Return the Scwm window object corresponding to GTKWIN.
GTKWIN is a GTk+ window object returned from `gtk-window-new'.
Return value is #f if no corresponding window exists now.
Also will return #f if your guile-gtk implementation does
not support this procedure (you should upgrade!)."
  (if (not (bound? gtk-window-get-window-id))
      #f
      (id->window (gtk-window-get-window-id gtkwin))))


(define (add-the-hook)
  (add-hook! error-hook gtk-show-error))

(define*-public (use-gtk-error-window-for-scwm #&optional (on #t))
  (if on
      (if (done-startup?)
	  (add-the-hook)
	  (append-hook! startup-hook add-the-hook))
      (if (done-startup?)
	  (remove-hook! error-hook gtk-show-error)
	  (remove-hook! startup-hook add-the-hook))))
  
(if (done-startup?) 
    (start-handling-gtk-events)
    (add-hook! startup-hook start-handling-gtk-events))

;;; Some GTK+ helper stuff

(define-public (gtk-clist-get-row-values clist row col)
  "Return a list of the COL columns of row ROW of CLIST."
  (let ((text (make-vector 1))
	(answer '()))
    (vector-set! text 0 "")
    (do ()
	((< col 0) answer)
      (gtk-clist-get-text clist row col text)
      (set! answer (cons (vector-ref text 0) answer))
      (set! col (- col 1)))))

(define-public (gtk-text-replace textwidget text)
  "Replace all the text in TEXTWIDGET with TEXT."
  (gtk-text-set-point textwidget 0)
  (gtk-text-forward-delete textwidget (gtk-text-get-length textwidget))
  (gtk-text-insert textwidget #f #f #f text (string-length text))
  (gtk-text-set-point textwidget 0))

(define-public (gtk-scrolled-window-set-vadjustment-value sw float)
  "Set the vadjustment for SW, a scrolled window, to FLOAT."
  (gtk-adjustment-set-value (gtk-scrolled-window-get-vadjustment sw) float))

(define-public (gtk-scrolled-window-set-hadjustment-value sw float)
  "Set the hadjustment for SW, a scrolled window, to FLOAT."
  (gtk-adjustment-set-value (gtk-scrolled-window-get-hadjustment sw) float))


;; (define f (gtk-window-new 'toplevel))
;; (gtk-window-get-window-id f)
;; (id->window (gtk-window-get-window-id f))
;; (gtk-window->scwm-window f)
