;; -*-Emacs-Lisp-*-
;; SCWM Interactive mode
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

;; SCWM interaction mode is for use with Epoch and the SCWM window
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

;; This can probably done using a set-property command in XEmacs/Epoch
(defun scwm-execute (arg)
  (interactive "s")
  (save-excursion
    (switch-to-buffer
     (get-buffer-create "*new-scwm-command*"))
    (insert-string arg)
    (shell-command-on-region (point-min) (point-max) "scwmsend -i")
    (kill-buffer "*new-scwm-command*")))

(defun scwm-execute-region  ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "scwmsend -i"))

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
