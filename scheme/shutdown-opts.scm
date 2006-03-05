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



(define-module (app scwm shutdown-opts)
  :use-module (app scwm base)
  :use-module (app scwm wininfo)
  :use-module (app scwm optargs))



(define opt-switch-to-home-viewport #f)
(define opt-move-windows-to-current-viewport #f)

(define*-public (shutdown-options #:key switch-to-home-viewport
				  move-windows-to-current-viewport)
  "Configure shutdown preferences.

Both SWITCH-TO-HOME-VIEWPORT and MOVE-WINDOWS-TO-CURRENT-VIEWPORT may
be be any of #f, #t, 'shutdown-only or 'restart-only. These options
indicate when scwm should switch to the (0 0) viewport on shutdown,
and whether all windows are moved to to the current viewport on
shutdown, respectively.  The default is #f.

The possible settings mean never, always, only when shutting down or
only when restarting, respectively.

Any window movement is done after any viewport switching."
  (if switch-to-home-viewport
      (set! opt-switch-to-home-viewport switch-to-home-viewport))
  (if move-windows-to-current-viewport
      (set! opt-move-windows-to-current-viewport
	    move-windows-to-current-viewport)))

(define (restarting-match opt restarting?)
  (or (eq? #t opt) 
      (if restarting? 
	  (eq? 'restart-only opt) 
	  (eq? 'shutdown-only opt))))

(define (shutdown-options-proc restarting?)
  (if (and (restarting-match opt-switch-to-home-viewport restarting?))
      (set-viewport-position! 0 0))
  (if (restarting-match opt-move-windows-to-current-viewport restarting?)
      (for-each (lambda (win)
		  (if (not (visible? win))
		      (let ((pos (window-viewport-position win)))
			(display (list "Moving to: "
				       (modulo (car pos) display-width)
				       (modulo (cadr pos) display-height)))
			(move-to
			 (modulo (car pos) display-width)
			 (modulo (cadr pos) display-height) win))))
	      (list-all-windows))))

(add-hook! shutdown-hook shutdown-options-proc)
