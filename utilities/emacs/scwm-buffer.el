;; -*-Emacs-Lisp-*-
;; SCWM Interactive mode
;; By Greg J. Badros <gjb@cs.washington.edu>
;; November 1997
;;
;; Significantly derived from code from standard lisp interaction
;; mode after being modified for GWM as gwm-buffer.el
;;
;; Original file's copyright information:
;; Copyright (C) 1992 Mike Fletcher 
;;			gt0293b@prism.gatech.edu, fletch@cad.gatech.edu,
;;			ccastmf@prism.gatech.edu

;;; File:		scwm-mode.el
;;; Description:	SCWM interactive editing mode
;;; Author:		Mike Fletcher <gt0293b@prism.gatech.edu>
;;;                     Modified for SCWM by Greg J. Badros <gjb@cs.washington.edu>
;;; Idea taken from:    Lisp interaction mode from std. distribution
;;; First created:	May 26, 1992 as gwm-buffer.el
;;; Last Modified:	October 19, 1997
;;; Version:		1.0

;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 1, or (at your option)
;;   any later version.

;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; SCWM interaction mode is for use with (X)Emacs and the SCWM window
;; manager.  It provides functionallity similar to the builtin
;; lisp-interaction-mode of Emacs for SCWM code.  Basically the
;; only change was making a new function (scwm-eval-last-sexp) to grab
;; the last sexp and send it to SCWM by way of the 'SCWM_EXECUTE' X
;; property.  See the SCWM manual for more details.

(if (not (boundp 'emacs-lisp-mode-map))	; Need to make sure standard 
    (load-library "lisp-mode"))		; lisp mode stuff has been loaded

(defvar scwm-interaction-mode-map ()
  "Keymap for SCWM interaction mode.")

(if scwm-interaction-mode-map		; If need to bind keys
    ()
  (setq scwm-interaction-mode-map (make-sparse-keymap))
;;  (lisp-mode-commands scwm-interaction-mode-map) ;;; commented out --gjb
  (define-key scwm-interaction-mode-map "\n" 'scwm-eval-last-sexp))

;; This part requires that the scwmsend program is built and is in your path
(defun scwm-send (start end)
  (shell-command-on-region start end "scwmsend -i"))

(defvar X-dpy-for-scwm nil)
(defvar X-property-scwm-exec nil)
(defvar X-display-number 1)
(defvar X-hostname (getenv "HOSTNAME"))
(defvar X-root-window nil)

;; use the $DISPLAY environment variable to figure out which
;; display to open
(let ((display-name (getenv "DISPLAY")))
  (if (string-match "\\(.*\\):\\(.*\\)" display-name)
      (progn
	(setq X-display-number (match-string 2 display-name))
	(setq X-hostname (match-string 1 display-name)))))

(defun string-to-list (string)
  (if (> (length string) 0)
      (append (list (string-to-char string)) (string-to-list (substring string 1)))
    nil))

;;; If you have Eric Ludlam's X for emacs package, this should
;;; work for you.  Be sure that scwm has already started (so that
;;; the SCWM_EXECUTE atom has already been interned) and that
;;; the display is accessible to the emacs process (easiest way to
;;; do this is xhost +<machinename-running-emacs>, but this is
;;; a giant security hole, too
;;; The advantage of this is that you don't have to wait 2 seconds
;;; for scwmsend to exec a new process every time you want to eval
;;; something
;;; Eric Ludlam's <zappo@gnu.ai.mit.edu> X for emacs is available from
;;; his home page at http://www.ultranet.com/~zappo/fsf.shtml
;;; This requires the patch in the scwm-buffer.el to be applied 
;;; to X-0.1.tar.gz in order to get the extra argument to XOpenDisplay
;;; and to get the XChangeProperty that takes a string as data
(condition-case nil
    (progn
      (require 'xlib)
      (setq X-dpy-for-scwm (XOpenDisplay X-hostname X-display-number))
      (setq X-property-scwm-exec (XInternAtom X-dpy-for-scwm "SCWM_EXECUTE" t))
      (setq X-root-window (X-window-alloc (X-RootWindow X-dpy-for-scwm 0) X-dpy-for-scwm))
      (defun scwm-send (start end) 
	(let ((string (buffer-substring-no-properties start end)))
	  (XChangeProperty X-dpy-for-scwm X-root-window
			   X-property-scwm-exec  
			   XA-string X-format-8 X-PropModeReplace string)
	  (message "Sent %s" string))))
  (error (message "Could not load xlib library; will try using scwmsend -i")))

(defun scwm-send (start end) 
  (let ((string (buffer-substring-no-properties start end)))
    (XChangeProperty X-dpy-for-scwm X-root-window
		     X-property-scwm-exec  
		     XA-string X-format-8 X-PropModeReplace string)
    (message "Sent %s" string)))


;; This can probably done using a set-property command in XEmacs/Epoch
(defun scwm-execute (arg)
  (interactive "s")
  (save-excursion
    (switch-to-buffer
     (get-buffer-create "*new-scwm-command*"))
    (insert-string arg)
    (scwm-send (point-min) (point-max))
    (kill-buffer "*new-scwm-command*")))

(defun scwm-execute-region  ()
  (interactive)
  (scwm-send (region-beginning) (region-end)))

(defun scwm-eval-last-sexp (arg)
  "Sends sexp before point to SCWM via the SCWM_EXECUTE property of the
window.  Output is sent to stderr of the SCWM process." 
  (interactive "P")
  (copy-to-register 24
    (let ((stab (syntax-table)))
      (unwind-protect 
	  (save-excursion
	    (set-syntax-table emacs-lisp-mode-syntax-table)
	    (forward-sexp -1)
	    (point))
	(set-syntax-table stab)))
    (point) ())
  (scwm-execute (get-register 24)))

(defun scwm-interaction-mode ()
  "Major mode for typing and evaluating SCWM code for the SCWM window
manager.  Mostly a direct rip off of Lisp-interaction mode from the
Emacs distribution.

Commands:
Same as Lisp-interaction mode, except LFD sends the current sexp to
SCWM to be executed (by means of the SCWM_EXECUTE property).
\\{scwm-interaction-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (use-local-map scwm-interaction-mode-map)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq major-mode 'scwm-interaction-mode)
  (setq mode-name "SCWM Interaction")
  (lisp-mode-variables ())
  (run-hooks 'scwm-interaction-mode-hook))

(defun scwm-buffer ()
  "Opens up a new buffer named *SCWM* in SCWM interaction mode."
  (interactive)
  (switch-to-buffer (get-buffer-create "*SCWM*"))
  (scwm-interaction-mode))

(load "scheme")
(define-key scheme-mode-map [(control meta ?x)] 'scwm-eval-last-sexp)
(define-key scwm-interaction-mode-map [(control meta ?x)] 'scwm-eval-last-sexp)
;; Use C-M-z to permit moving the mouse before sending the string
;; (useful for testing current-window-with-{pointer,focus})
(define-key scwm-interaction-mode-map [(control meta ?z)] 
  (function (lambda ()
	      (interactive)
	      (sleep-for 1)
	      (call-interactively 'scwm-eval-last-sexp))))
