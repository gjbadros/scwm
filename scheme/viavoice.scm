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



(define-module (app scwm viavoice)
  :use-module (app scwm viavoice-smrc)
  :use-module (app scwm scwmviavoice)
  :use-module (app scwm optargs)
  :use-module (ice-9 regex)
  :use-module (app scwm base)
  :use-module (app scwm winlist)
  :use-module (app scwm task-switcher)
  :use-module (app scwm tile)
  :use-module (app scwm cascade)
  :use-module (app scwm undo)
  :use-module (app scwm window-selection)
  :use-module (app scwm winops)
  :use-module (app scwm animation)
  :use-module (app scwm animated-edge-moves)
  :use-module (app scwm animated-iconify)
  :use-module (app scwm defoption)
  )


;; (vv-smrc->error -8)
(define-public (vv-smrc->error value)
  (assoc-ref vv-smrc-errors value))

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
  (system (string-append (getenv "SPCH_BIN") "/vvuseradm -makedirs")))

;; (vv-use-grammar "scwmgrammar.fsg")
(define*-public (vv-use-grammar #&optional (name "scwmgrammar")
				(file 
				 (string-append 
				  (scwm-path-prefix) 
				  "/share/scwm/ViaVoice/scwmgrammar.fsg")))
  (if (not (vv-connected?))
      (error "ViaVoice not connected"))
  (vv-define-grammar name file)
  (vv-enable-vocab name))

;; scwmviavoice
;; (vv-define-grammar "foo" "/usr/share/scwm/ViaVoice/scwmgrammar.fsg")
;; (vv-define-grammar "bar" "/home/gjb/scwm/modules/viavoice/scwmgrammar.fsg")
;; (vv-define-grammar "baz" "/home/gjb/scwm/modules/viavoice/scwmgrammar.fsg")
;; (vv-define-grammar "bong" "/home/gjb/scwm/modules/viavoice/scwmgrammar.fsg")
;; (vv-define-grammar "new" "/home/gjb/scwm/modules/viavoice/scwmgrammar.fsg")
;; (vv-enable-vocab "new")

(define-public (vv-setup-recognition-hook)
  (reset-hook! vv-recognition-hook)
  (add-hook! vv-recognition-hook vv-recognition-actions)
  (add-hook! vv-recognition-hook vv-recognition-debug))

(define-public (vv-terminate)
  "Use this to terminate ViaVoice recognition.
This will release /dev/dsp for other uses.  Use
`vv-initialize' to restart (when /dev/dsp is no
longer in use by another process."
  (if (vv-connected?)
      (begin
	(vv-disconnect)
	(vv-close))))

;; (vv-connected?)
;; (vv-connect (lambda (status) (vv-use-grammar "/home/gjb/scwm/modules/viavoice/scwmgrammar.fsg")))

;; viavoice
;; scwmviavoice
(define-public (vv-initialize)
  "Use this to start ViaVoice recognition."
  (vv-initialize-environment)
  (vv-connect (lambda (status) (vv-use-grammar (string-append (scwm-path-prefix) "/share/scwm/ViaVoice/scwmgrammar.fsg"))))
  (vv-turn-microphone-on)
  (vv-setup-recognition-hook)
  )

(define-public (vv-recognition-actions accepted? command annotations)
  (define (matches-command? substring)
    (string-match substring command))
  (if accepted?
      (let ((win (or (window-with-pointer))))
	(display "got accepted command ")
	(display command)
	(newline)
	(if win
	    (let* ((pos (window-position win))
		   (x (car pos))
		   (y (cadr pos))
		   (do-animation #f)
		   (amount (or (and (> (vector-length annotations) 3) (array-ref annotations 3)) 100)))
	      ;;; WARNING: be sure that the longer strings come first.
	      ;;; Should just do this more logically, anyway
	      (cond
	       ((matches-command? "north west") (animated-move-to-nw win))
	       ((matches-command? "north east") (animated-move-to-ne win))
	       ((matches-command? "south west") (animated-move-to-sw win))
	       ((matches-command? "south east") (animated-move-to-se win))
	       ((matches-command? "right") (set! x (+ x amount)) (set! do-animation #t))
	       ((matches-command? "left") (set! x (- x amount)) (set! do-animation #t))
	       ((matches-command? "down") (set! y (+ y amount)) (set! do-animation #t))
	       ((matches-command? "up") (set! y (- y amount)) (set! do-animation #t))
	       ((matches-command? "north") (animated-move-to-n win))
	       ((matches-command? "west") (animated-move-to-w win))
	       ((matches-command? "east") (animated-move-to-e win))
	       ((matches-command? "south") (animated-move-to-s win))
	       ((matches-command? "cotton") (animated-window-shade win))
	       ((matches-command? "close") (close-window win))
	       ((matches-command? "window select") (select-window-toggle win))
	       ((matches-command? "minimize") (animated-iconify win))
	       ((matches-command? "shade") (animated-toggle-window-shade win))
	       ((matches-command? "maximize") (toggle-maximize-vertical win)))
	      (if do-animation
		  (begin
		    (for-each display 
			      (list "move window " win " "
				    command " to " x " " y " " amount "\n"))
		    (animated-move-window x y win #t))))
	    (display "no current window\n"))
	(cond
	 ((matches-command? "focus previous") (prev-visible-non-iconified-window))
	 ((matches-command? "focus next") (next-visible-non-iconified-window))
	 ((matches-command? "select next") (next-window))
	 ((matches-command? "select previous") (prev-window))
	 ((matches-command? "list") (window-task-switcher-menu))
	 ((matches-command? "term") (start-xterm))
	 ((matches-command? "logo") (start-xlogo))
	 ((matches-command? "emacs") (execute "xemacs"))
	 ((matches-command? "tile") (if (not (null? (selected-windows-list))) (tile-windows-interactively)))
	 ((matches-command? "cascade") (cascade-windows (selected-windows-list)))
	 ((matches-command? "undo") (undo))
	 ((matches-command? "remember") (push-undo-global))
	 ))
      (begin
	(display "Rejected ")
	(display command)
	(newline))))

(define-public (vv-recognition-debug accepted? word annotation)
  (for-each display (list "Got " (if accepted? "accepted" "rejected") 
			  " word = " word ", annotation = " annotation "\n")))
      
;;(add-hook! vv-recognition-hook vv-move-window)
;;(add-hook! vv-recognition-hook vv-recognition-debug)
;;(remove-hook! vv-recognition-hook vv-move-window)
;;(reset-hook! vv-recognition-hook)

(vv-initialize)
