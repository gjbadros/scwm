;;;; $Id$
;;;; Copyright (C) 1999 Greg J. Badros
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
;;;; property-respond.scm

(define-module (app scwm property-respond)
  :use-module (app scwm stringops)
  :use-module (app scwm optargs)
  :use-module (app scwm flash-window)
  :use-module (app scwm base))


;; (use-scwm-modules property-respond)
;; (start-property-respond)
;; (stop-property-respond)
;; (reset-hook! X-PropertyNotify-hook)

;; echo -n "\033]3;flashing=true\a"
;; echo -n "\033]3;flashing\a"          # turn off flashing
;; echo -n "\033]3;flashing=yellow\a"
;; echo -n "\033]3;flash=true\a"        # flash once

;; (use-scwm-modules stringops optargs base)
;; (define w (select-window-interactively))
;; (X-property-get w "flash")

(define-public window-flashing-start-hook (make-hook 1))
(define-public window-flashing-stop-hook (make-hook 1))

(define-public (property-changed-debug prop win)
  "Print debugging information about the property change of PROP on WIN.
See also `X-PropertyNotify-hook'."
  (write-all #t win " changed " prop " to " 
	     (X-property-get win (string->X-atom prop)) "\n"))

;; (add-hook! X-PropertyNotify-hook property-changed-debug)
;; (reset-hook! X-PropertyNotify-hook)

(define-public (property-changed-respond prop win)
  "Handle various property changes of PROP on WIN.
See also `X-PropertyNotify-hook'.  Currently handles
\"flashing\" and \"flash\"."
  (let* ((value (X-property-get win (string->X-atom prop)))
	 (color-name (and value (car value)))
	 (color (and color-name (maybe-make-color color-name))))
    (case (string->symbol prop)
      ('flashing
       (if value
	   (begin
	     (flash-window win #:continually #t #:color color)
	     (run-hook window-flashing-start-hook win))
	   (begin
	     (stop-flashing-window win)
	     (run-hook window-flashing-stop-hook win))))
      ('flash
       (if value
	   (flash-window win #:color color)))
      (else #f))))

(define-public (start-property-respond)
  "Turn on property-change responses.
See `property-changed-respond' and `X-PropertyNotify-hook'."
  (add-hook! X-PropertyNotify-hook property-changed-respond))

(define-public (stop-property-respond)
  "Turn off property-change responses.
See `property-changed-respond' and `X-PropertyNotify-hook'."
  (if (bound? property-changed-debug)
      (remove-hook! X-PropertyNotify-hook property-changed-debug))
  (if (bound? property-changed-respond)
      (remove-hook! X-PropertyNotify-hook property-changed-respond)))
