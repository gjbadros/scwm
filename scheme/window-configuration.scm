;;;; $Id$
;;;; (C) 1999 Greg J. Badros
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
  :use-module (app scwm base)
  :use-module (app scwm winlist))

(define*-public (window-configuration #&optional (w (get-window)))
  (with-window 
   w
   (list
    w					; 0
    (window-id)				; 1
    (window-position)			; 2
    (window-frame-size)))		; 3
  )

;; (define c (window-configuration))

(define*-public (copy-window-configuration configuration #&optional (w (get-window)))
  (with-window
   w
   (apply move-window (list-ref configuration 2))
   (apply resize-frame-to (list-ref configuration 3))))

;; (copy-window-configuration c)

(define*-public (restore-window-configuration global-configuration 
					     #&optional (w (get-window)))
  (let ((c (assoc w global-configuration)))
    (if c (begin (copy-window-configuration c w) #t) #f)))

(define-public (global-window-configuration)
  (map window-configuration (list-stacking-order)))

;; (define gc (global-window-configuration))

(define-public (restore-global-window-configuration global-configuration)
  (map (lambda (w) (restore-window-configuration global-configuration w))
       (list-stacking-order))
  (restack-windows (map car global-configuration)))
