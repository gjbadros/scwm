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




(define-module (app scwm auto-unobscure)
  :use-module (app scwm optargs)
  :use-module (app scwm defoption)
  :use-module (app scwm style)
  :use-module (app scwm style-options)
  :use-module (app scwm module-types))



;; (window-style "XTerm" #:auto-unobscure #t)
;; (window-style "XLogo" #:auto-unobscure #t)
;; (window-style "XLogo" #:auto-unobscure-proc smart-place-window)
;; (window-style "XLogo" #:auto-unobscure-proc clever-place-window)

(define*-public (set-auto-unobscure! auto-unobscure? #&optional (win (get-window)))
  "Turn auto-unobscure on (#t) or off (#f) for WIN.
Auto-unobscure makes a window automatically move when it becomes
fully obscured."
  (if win (set-window-property! win 'auto-unobscure auto-unobscure?)))

(define*-public (set-auto-unobscure-proc! fproc #&optional (win (get-window)))
  "Set the auto-unobscure-proc for WIN.
The auto-unobscure-proc is the procedure which is invoked
when WIN is fully obscured if auto-unobscuring is set for the window."
  (if win (set-window-property! win 'auto-unobscure-proc fproc)))

;;; Default proc to use to unobscure windows.
(define-public default-auto-unobscure-move-proc smart-place-window)

(define (auto-unobscure-hook-proc win resulting-from-viewport-move?)
  (if (and win 
           (not resulting-from-viewport-move?)
	   (window-property win 'auto-unobscure)
	   (not (eq? (window-with-pointer) win)))
      ((or (window-property win 'auto-unobscure-proc)
	   default-auto-unobscure-move-proc) win)))

(define-public (enable-auto-unobscure)
  "Enable auto-unobscure for windows with #:auto-unobscure on.
Works really nicely with 'clever-place-window'."
  (add-hook! window-fully-obscured-hook auto-unobscure-hook-proc))

;; (reset-hook! window-fully-obscured-hook)
(define-public (disable-auto-unobscure)
  "Disable auto-unobscure for all windows."
  (remove-hook! window-fully-obscured-hook auto-unobscure-hook-proc))

(add-window-style-option #:auto-unobscure set-auto-unobscure!)
(add-window-style-option #:auto-unobscure-proc set-auto-unobscure-proc!)

(provide 'scwm-auto-unobscure)
