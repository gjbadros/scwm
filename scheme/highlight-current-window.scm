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


(define-module (app scwm highlight-current-window)
  :use-module (app scwm optargs)
  :use-module (app scwm window-selection)
  :use-module (app scwm flash-window)
  :use-module (app scwm modifier-key-bindings))


(define current-highlighted-window #f)

(define (flash-window-on-and-update-current-highlighted win)
  (set! current-highlighted-window (flash-window-on win)))

(define* (unflash-window-if-not-selected #&optional (win (window-context)))
  "Unflash WIN as long as it is not selected."
  (if (and win (not (window-is-selected? win)))
      (begin (unflash-window win)
	     (set! current-highlighted-window #f))))

(define-public (start-highlighting-selected-window)
  "Highlight the current window during window selections."
  (add-hook! select-window-enter-hook flash-window-on)
  (add-hook! select-window-leave-hook unflash-window-if-not-selected)
  (add-hook! select-window-done-hook unflash-window-if-not-selected))

(define-public (end-highlighting-selected-window)
  "Stop highlighting the current window during window selections."
  (remove-hook! select-window-enter-hook flash-window-on)
  (remove-hook! select-window-leave-hook unflash-window-if-not-selected)
  (remove-hook! select-window-done-hook unflash-window-if-not-selected))

(define*-public (start-highlighting-current-window #&optional (win (window-context)))
  "Add appropriate hook procedures to make the window with the mouse be highlighted.
See also `end-highlighting-current-window'."
  (and win (set! current-highlighted-window (flash-window-on win)))
  (add-hook! window-enter-hook flash-window-on-and-update-current-highlighted)
  (add-hook! window-leave-hook unflash-window-if-not-selected))

;; window-leave-hook  window-enter-hook

(define*-public (end-highlighting-current-window 
		 #&optional (win current-highlighted-window))
  "Remove the hook procedures that make the window with mouse be highlighted.
See also `start-highlighting-current-window'."
  (and win (unflash-window-if-not-selected win))
  (remove-hook! window-enter-hook flash-window-on-and-update-current-highlighted)
  (remove-hook! window-leave-hook unflash-window-if-not-selected))

(define-public (highlight-current-window . modifiers)
  "Turn on the keylighting of the current window.  
MODIFIERS should be two or three keycode + modifier mask integer cons
pairs corresponding to the modifier key conjunction that you want to
turn on highlighting of the current window.  The modifier-key-bindings
module defines some symbolic constants for the MODIFIERS arguments
(e.g., XKM_SHIFT_L)."
  (let ((len (length modifiers))
	(args (append modifiers
		      (list start-highlighting-current-window 
			    end-highlighting-current-window))))
    (case len
      ((2) (apply bind-two-modifier-key-events args))
      ((3) (apply bind-three-modifier-key-events args)))))

;; (highlight-current-window XKM_HYPER_L XKM_SHIFT_L)

