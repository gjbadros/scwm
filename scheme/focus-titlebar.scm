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

(define-module (app scwm focus-titlebar)
  :use-module (app scwm defoption)
  :use-module (app scwm optargs)
  :use-module (app scwm winops))

(define (show-titlebar-in-place-focussed-window w)
  (and w (window-valid? w) (show-titlebar-in-place w)))

;; GJB:FIXME:: Make this stuff interact better
;; with window-styles
(define-public (turn-on-only-focus-window-has-titlebar)
  "Start displaying a titlebar only on the focussed window.
This forces titlebars off all other windows (ignoring window-style
commands)."
  (add-hook! window-focus-change-hook show-titlebar-in-place-focussed-window)
  (add-hook! window-focus-lost-hook hide-titlebar-in-place)
  (for-each hide-titlebar-in-place (list-all-windows)))

(define-public (turn-off-only-focus-window-has-titlebar)
  "Do not display titlebars on only the focuessed window.
This forces titlebars on all other windows (ignoring window-style
commands)."
  (remove-hook! window-focus-change-hook show-titlebar-in-place-focussed-window)
  (remove-hook! window-focus-lost-hook hide-titlebar-in-place)
  (for-each show-titlebar-in-place-focussed-window (list-all-windows)))
  
(define-scwm-option *only-focus-window-has-titlebar* #f
  "If #t, no windows except the focus window will have a titlebar."
  #:type 'boolean
  #:group 'focus
  #:setter (lambda (v)
	     (or
	      (and v (not *only-focus-window-has-titlebar*) 
		   (turn-on-only-focus-window-has-titlebar))
	      (and (not v) *only-focus-window-has-titlebar* 
		   (turn-off-only-focus-window-has-titlebar)))
	     (set! *only-focus-window-has-titlebar* v))
  #:getter (lambda () *only-focus-window-has-titlebar*))
