;;; File: <scwm.el - 1998-03-03 Tue 08:33:34 EST sds@mute.eaglets.com>
;;;
;;; Copyright (c) 1998 by Sam Shteingold <sds@usa.net>
;;;
;;; Completion-support added by Greg J. Badros <gjb@cs.washington.edu>
;;;    03/11/98 gjb
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

;;; Intructions to the user; put this file somewhere in your load-path
;; and add the following lines in your .emacs:

;; (load "scwm")
;; (define-key scheme-mode-map "\C-j" 'scwm-eval-print)
;; (define-key scheme-mode-map "\C-x\C-j" 'scwm-eval-to-minibuffer)
;; (define-key scheme-mode-map [tab] 'scwm-complete-symbol)

;; Now you do M-x scwm-run to get the *scwm* buffer, where you can type
;; commands to be sent to scwm; while you can type C-x C-e in your
;; scheme buffers to send the last SEXP to the interpreter. See C-h v
;; scheme-buffer for details on running several scheme interpreters at
;; once.

;; *Alternatively* (that's OR, not XOR) you can type C-j evaluate the
;; last SEXP and insert the results at point, or type C-x C-j to display
;; them in the minibuffer.  This functionality does not require
;; preliminary M-x scwm-run.  Note that you can find your recent
;; minibuffer messages in the buffer *Messages*.

(defvar scwm-repl "/usr/local/bin/scwmrepl" "The path to scwmrepl.")

(defvar scwm-exec "/usr/local/bin/scwmexec" "The path to scwmexec.")

(defun scwm-run ()
  "Run scwm interaction.
Use \\[scheme-send-last-sexp] to eval the last sexp there."
  (interactive)
  (run-scheme scwm-repl)
  (set-buffer scheme-buffer)
  (rename-buffer "*scwm*")
  (setq scheme-buffer "*scwm*"))

(defun scwm-eval-print ()
  "Evaluate the last SEXP and insert the result into the current buffer."
  (interactive) (newline-and-indent)
  (call-process scwm-exec nil t nil
		(buffer-substring-no-properties
		 (point) (save-excursion (backward-sexp) (point))))
  (newline))

(defun scwm-eval-to-minibuffer ()
  "Evaluate the last SEXP and show the result in the minibuffer."
  (interactive)
  (let* ((start (point))
	 (end (save-excursion (backward-sexp) (point)))
	 (arg (buffer-substring-no-properties start end))
	 (string (with-output-to-string
		   (call-process scwm-exec nil standard-output nil arg))))
    (message string)))

(defun make-list-of-symbols ()
  "Return a list of all the s-expressions in the current buffer from point on"
  (let ((sexp-list nil)
	(old-point (point))
	(done nil))
    (while (not done)
      (forward-sexp)
      (if (looking-at ")")
	  (setq done 't))
      (setq sexp-list (cons (intern (buffer-substring-no-properties old-point (point))) sexp-list))
      (setq old-point (+ 1 (point))))
    sexp-list))

;; This is rough, but can be the foundation of something
;; a lot fancier and cleaner
(defun scwm-complete-symbol ()
  "Complete the current symbol by querying scwm using apropos-internal."
  (interactive)
  (let* ((start (point))
	 (end (save-excursion (forward-word -1) (point)))
	 (arg (buffer-substring-no-properties start end))
	 (bfr (get-buffer-create "*scwm-completions*"))
	 (choices (save-excursion
		     (set-buffer bfr)
		     (erase-buffer)
		     (lisp-mode)
		     (call-process scwm-exec nil t nil (concat "(apropos-internal \"^" arg "\")"))
		     (goto-char 2)
		     (make-list-of-symbols))))
    ;; This cannot possibly be right, but it works for now
    (completer-complete-goto
     "^ \t\n\(\)[]{}'`" completer-words (vconcat choices) (lambda (test) t))))

;;; Make C-x C-e do the right thing
(defun advertised-xscheme-send-previous-expression ()
  (interactive)
  (scwm-eval-to-minibuffer))
