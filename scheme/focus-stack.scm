;;;; $Id$
;;;; Copyright (C) 2000 Greg J. Badros
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

(define-module (app scwm focus-stack)
  :use-module (app scwm defoption)
  :use-module (app scwm optargs)
  :use-module (app scwm message-window)
  :use-module (app scwm winops))


(define-public focus-stack '())

(define*-public (push-focus-window)
  "Save the current focus window on a stack.
See `pop-focus-window'."
  (interactive)
  (let ((w (window-with-focus)))
    (if w
	(set! focus-stack (cons w focus-stack)))))

(define*-public (pop-focus-window)
  "Restore the focus to the window on the top of the focus-stack"
  (interactive)
  (if (not (focus-stack-empty?))
      (let ((w (car focus-stack)))
        (set! focus-stack (cdr focus-stack))
        (if (window-valid? w)
            (focus-change-warp-pointer w)
            (pop-focus-window)))
      (display-message-briefly "Empty focus stack")))

(define*-public (focus-stack-empty?)
  "Return #t iff the focus-stack is empty, else #f."
  (null? focus-stack))

(define*-public (close-window-and-pop-focus #&optional (win (get-window)))
  "Close WIN and pop the focus with `pop-focus-window'."
  (interactive)
  (delete-window win)
  (add-timer-hook! 100 (lambda () (pop-focus-window))))
