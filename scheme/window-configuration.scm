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


;; I do not claim that all the properties that we might
;; like to save are saved, but this code is already
;; useful.

(define-module (app scwm window-configuration)
  :use-module (app scwm wininfo)
  :use-module (app scwm optargs)
  :use-module (app scwm animation)
  :use-module (app scwm listops)
  :use-module (app scwm base))

(define*-public (window-configuration #&optional (win (get-window)))
  "Return a list containing the state of WIN."
  (with-window 
   win
   (list
    win					; 0
    (window-id)				; 1
    (window-position)			; 2
    (window-frame-size)))		; 3
  )

(define-public (window-configuration? wcfg)
  "Return #t if WCFG is a window configuration."
  (and (pair? wcfg)
       (window? (car wcfg))
       (pair? (cdr wcfg))
       (number? (cadr wcfg))))

;; (define c (window-configuration))

;; GJB:FIXME:: make animation optional.
(define*-public (copy-window-configuration configuration #&optional (win (get-window)))
  "Apply a saved state CONFIGURATION to window WIN."
  (apply animated-resize-frame
	 (append (list-ref configuration 3) (list win)
		 (list-ref configuration 2))))

;; (copy-window-configuration c)

(define*-public (restore-window-configuration global-configuration 
					     #&optional (win (get-window)))
  "Restore the state of WIN from GLOBAL-CONFIGURATION."
  (let ((c (assoc win global-configuration)))
    (if c (begin (copy-window-configuration c win) #t) #f)))

(define-public (global-window-configuration)
  "Return an object abstracting all of the current windows' states."
  (map window-configuration (list-stacking-order)))

;; (define gc (global-window-configuration))
;; (define win (get-window))
;; (define cfg (window-configuration (get-window)))
;; (window-configuration->xform-to-it cfg)

(define*-public (window-configuration->xform-to-it cfg)
  "Create a transformation element for going from the current state to CFG.
CFG should be a window configuration object.  See also `animate-windows'."
  (let* ((win (car cfg))
	 (pos (window-viewport-position win))
	 (size (window-frame-size win))
	 (startX (car pos))
	 (startY (cadr pos))
	 (startW (car size))
	 (startH (cadr size))
	 (endPos (caddr cfg))
	 (endSize (cadddr cfg))
	 (endX (vx->vpx (car endPos)))
	 (endY (vy->vpy (cadr endPos)))
	 (endW (car endSize))
	 (endH (cadr endSize))
	 )
    (list win #t
	  (cons startW startH)
	  (cons endW endH)
	  (cons startX startY)
	  (cons endX endY)
	  (cons #f #f))))

(define-public (restore-global-window-configuration global-configuration)
  "Restore the states of all windows from GLOBAL-CONFIGURATION."
  (animate-windows
   (filter-map 
    (lambda (w) 
      (let ((cfg (assoc w global-configuration)))
	(if cfg
	    (window-configuration->xform-to-it cfg)
	    #f)))
    (list-stacking-order)))
  (restack-windows (map car global-configuration)))

(define*-public (push-window-configuration #&optional (win (get-window)))
  "Save the configuration of WIN on its stack of previous configurations."
  (interactive)
  (set-window-property! 
   win 'window-configuration-stack
   (cons 
    (window-configuration win)
    (or (window-property win 'window-configuration-stack) '()))))

				      
(define*-public (pop-window-configuration #&optional (win (get-window)))
  "Restore the last configuration of WIN that was saved on its stack of previous configurations."
  (interactive)
  (let ((config (window-property win 'window-configuration-stack)))
    (if (and config (not (null? config)))
	(begin
	  (copy-window-configuration (car config) win)
	  (set-window-property! 
	   win
	   'window-configuration-stack 
	   (cdr config))))))

(define-public window-configuration-menu
  (menu
   (list
    (menu-title "Window configuration")
    menu-separator
    (menuitem "&Push" #:action push-window-configuration)
    (menuitem "P&op" #:action pop-window-configuration))))

;; (push-window-configuration)
;; (pop-window-configuration)
;; (popup-menu window-configuration-menu)
