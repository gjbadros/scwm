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



(define-module (app scwm viavoice)
  :use-module (app scwm scwmviavoice)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm defoption)
  )



;;; Modified from vvsetenv from IBM's distribution
(define-public (vv-initialize-environment)
  ;; Executables, shared libraries, tools, etc
  (setenv "SPCH_BIN" "/usr/lib/ViaVoice/bin")
  (setenv  "SPCH_TRN" "/usr/lib/ViaVoice/bin")

  ;; Data, lots and lots of data!!
  (setenv  "SPCH_RO" "/usr/lib/ViaVoice/vocabs")
  (setenv  "SPCH_LOC" "vl1")

  ;; User-specific things (in $HOME/viavoice)
  (setenv  "SPCH_RW" (string-append (getenv "HOME") "/viavoice/"))
  (setenv  "SPCH_RUN" (string-append (getenv "HOME") "/viavoice/temp"))

  ;; Make sure the home directory is set up
  ;; (this will create $HOME/viavoice/temp
  ;; and $HOME/viavoice/users)
  (system (string-append (getenv "SPCH_BIN") "/vvuseradm -makedirs"))
  )

(vv-initialize-environment)

(define (vv-move-window dir amount)
  (let ((win (current-window-with-focus)))
    (if win
	(let* ((pos (window-position win))
	       (x (car pos))
	       (y (cadr pos)))
	  (cond
	    ((string=? dir "right") (set! x (+ x amount)))
	    ((string=? dir "left") (set! x (- x amount)))
	    ((string=? dir "down") (set! y (+ y amount)))
	    ((string=? dir "up") (set! y (- y amount))))
	  (display "move window ")
	  (display win)
	  (display " ")
	  (display dir)
	  (display " to ")
	  (display x)
	  (display " ")
	  (display y)
	  (display " ")
	  (display amount)
	  (newline)
	  (animated-move-window x y win #t)))))
      
(add-hook! vv_recognition_hook vv-move-window)

;;(remove-hook! vv_recognition_hook vv-move-window)
;;(reset-hook! vv_recognition_hook)
