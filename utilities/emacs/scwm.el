;;; File: <scwm.el - 1998-03-23 Mon 16:23:02 EST sds@mute.eaglets.com>
;;;
;;; Copyright (c) 1998 by Sam Shteingold <sds@usa.net>
;;; $Id$
;;;
;;; Completion-support added by Greg J. Badros <gjb@cs.washington.edu>
;;;    03/11/98 gjb
;;;
;;; Completition, help and apropos are completely re-worked by sds
;;;	1998-03-13 Fri 13:58:16 EST	sds
;;;
;;; Fixed scwm-run to restart in the same buffer after a crash.
;;;	1998-03-17 Tue 15:50:55 EST	sds
;;;
;;; This file is distributed under the GPL. See
;;;	<URL:http://www.gnu.ai.mit.edu/copyleft/gpl.html>
;;; for further details.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with This file; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Intructions to the user; put this file somewhere in your load-path
;; and add the following lines in your .emacs:

;; (require 'scwm)

;; Now you do M-x scwm-run (C-c C-s) to get the *scwm* buffer, where you
;; can type commands to be sent to scwm; while you can type C-x C-e in
;; your scheme buffers to send the last SEXP to the interpreter.
;; See C-h v scheme-buffer for details on running several scheme
;; interpreters at once.

;; *Alternatively* (that's OR, not XOR) you can type C-j evaluate the
;; last SEXP and insert the results at point, or type C-x C-j to display
;; them in the minibuffer.  This functionality does not require
;; preliminary M-x scwm-run.  Note that you can find your recent
;; minibuffer messages in the buffer *Messages*.  Help and Apropos are
;; also available: type C-h C-a for apropos and C-h C-s for documantation.
;; Type M-TAB to complete symbol at point.

;; Note that this uses `with-output-to-string', which is broken in
;; XEmacs and absent from Emacs 19.  File lisp/subr.el from the Emacs
;; distribution contains the corrent version.  You can get the file from
;; http://sourcery.naggum.no.

;;; user variables
;;; ---- ---------

(defvar scwm-repl "scwmrepl" "The path to scwmrepl.")
(defvar scwm-exec "scwmexec" "The path to scwmexec.")

;;; pacify the compiler (XEmacs only)
(eval-and-compile (autoload 'id-select-symbol "id-select"))

;;; XEmacs doesn't have thingatpt. Too bad.
(unless (fboundp 'thing-at-point)
  (defun thing-at-point (what)
    "Return the thing at point."
    (unless (eq what 'symbol)
      (error "crippled `thing-at-point' - symbols only"))
    (let ((zz (id-select-symbol (point))))
      (buffer-substring-no-properties (car zz) (cdr zz)))))

;;; user functions
;;; ---- ---------

;;;###autoload
(defun scwm-run ()
  "Run scwm interaction or pop to an existing one.
Use \\[scheme-send-last-sexp] to eval the last sexp there."
  (interactive)
  (pop-to-buffer (setq scheme-buffer (make-comint "scwm" scwm-repl))))

(defsubst scwm-eval (sexp out)
  "Evaluate the SEXP with scwm-exec and print the results to OUT."
  (call-process scwm-exec nil out nil sexp))

(defsubst scwm-eval-last (out)
  "Evaluate the last sexp with scwm-exec and print the results to OUT."
  (scwm-eval (buffer-substring-no-properties
	      (point) (save-excursion (backward-sexp) (point))) out))

;;;###autoload
(defun scwm-eval-print ()
  "Evaluate the last SEXP and insert the result into the current buffer."
  (interactive) (newline-and-indent)
  (scwm-eval-last t) (newline))

;;;###autoload
(defun scwm-eval-to-minibuffer ()
  "Evaluate the last SEXP and show the result in the minibuffer."
  (interactive)
  (let ((last (buffer-substring-no-properties
	       (point) (save-excursion (backward-sexp) (point)))))
    (message (with-output-to-string (scwm-eval last standard-output)))))

(defalias 'advertised-xscheme-send-previous-expression
    'scwm-eval-to-minibuffer)

;;; completion
;;; ----------

(defvar scwm-obarray nil "The obarray for scwm completion.")
(defvar scwm-history nil "The input history of SCWM completions.")

(defun scwm-make-obarray ()
  "Create and return an obarray of SCWM symbols."
  ;; can't use read-from-string because "? " is read as 32
  (let ((obarray (make-vector 67 0)) (pos 2)
	(tb (get-buffer-create " *scwm-obarray*")))
    (set-buffer tb) (erase-buffer)
    (scwm-eval "(apropos-internal \"*\")" tb)
    (goto-char 1)
    (while (search-forward " " nil t)
      (intern (buffer-substring-no-properties pos (1- (point))) obarray)
      (setq pos (point)))
    obarray))

(defun scwm-complete-symbol (&optional sym)
  "Complete the current symbol by querying scwm using apropos-internal.
Returns a string."
  (setq sym (or sym (thing-at-point 'symbol))
	scwm-obarray (or scwm-obarray (scwm-make-obarray)))
  (completing-read "SCWM symbol: " scwm-obarray nil nil sym scwm-history sym))

;;;###autoload
(defun scwm-complete-symbol-insert ()
  (interactive)
  (let* ((end (point)) (beg (save-excursion (backward-sexp) (point)))
	 (pat (buffer-substring-no-properties beg end))
	 (comp (try-completion pat scwm-obarray)))
    (cond ((eq comp t) (message "`%s' is complete" pat))
	  ((null comp) (message "Cannot complete `%s'" pat) (ding))
	  ((not (string= comp pat)) (delete-region beg end) (insert comp))
	  (t (message "Making completion list...")
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list (all-completions pat scwm-obarray)))
	     (message "Making completion list...done")))))

;;; help
;;; ----

;;;###autoload
(defun scwm-documentation (pat)
  "Query scwm for documentation for the give symbol."
  (interactive (list (scwm-complete-symbol)))
  (with-output-to-temp-buffer "*Help*"
    (princ "SCWM documentation `") (princ pat) (princ "':\n\n")
    (scwm-eval (concat "(procedure-documentation " pat ")")
	       standard-output)))

;;;###autoload
(defun scwm-apropos (pat)
  "Apropos an scwm symbol."
  ;; (interactive (interactive-token "SCWM Apropos"))
  (interactive
   (list (read-string
	  "SCWM Apropos: "
	  (format "%s" (or (thing-at-point 'symbol)
			   (progn (skip-syntax-backward (or skip "^w"))
				  (if (bobp) () (backward-char 1))
				  (thing-at-point 'symbol)) "")))))
  (with-output-to-temp-buffer "*Apropos*"
    (princ "SCWM apropos `") (princ pat) (princ "':\n\n")
    (scwm-eval (concat "(apropos \"" pat "\")") standard-output)))

;;; keybindings
;;; -----------

(define-key scheme-mode-map "\C-j" 'scwm-eval-print)
(define-key scheme-mode-map "\C-c\C-s" 'scwm-run)
(define-key scheme-mode-map "\C-x\C-j" 'scwm-eval-to-minibuffer)
(define-key scheme-mode-map "\C-h\C-s" 'scwm-documentation)
(define-key scheme-mode-map "\C-h\C-a" 'scwm-apropos)
(define-key scheme-mode-map [(meta tab)] 'scwm-complete-symbol-insert)

(provide 'scwm)
;; scwm.el ends here
