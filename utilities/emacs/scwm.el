;;; scwm --- functions for editing and running SCWM code under Emacs

;; Copyright (c) 1998 by Sam Steingold <sds@usa.net>

;; File: <scwm.el - 1998-07-09 Thu 13:03:54 EDT sds@mute.eaglets.com>
;; Author: Sam Steingold <sds@usa.net>
;; Version: $Revision$
;; Keywords: language lisp scheme scwm

;; LCD Archive Entry:
;; scwm|Sam Steingold|sds@usa.net|
;; Functions for editing and running SCWM code under Emacs|
;; $Date$|$Revision$||

;;; $Id$

;;; History:

;; Completion-support added by Greg J. Badros <gjb@cs.washington.edu>
;;    03/11/98 gjb
;;
;; Completition, help and apropos are completely re-worked by sds
;;	1998-03-13 Fri 13:58:16 EST	sds
;;
;; Fixed scwm-run to restart in the same buffer after a crash.
;;	1998-03-17 Tue 15:50:55 EST	sds
;;
;; Made into a major mode.
;;	1998-04-16 Thu 10:38:19 CEST	robbe@orcus.priv.at
;;
;; Added font-lock support for the major mode stuff.
;;	1998-04-16 Thu 17:04:43 EDT	sds
;;
;; Added font-lock support for XEmacs and Emacs19 compatibility.
;;	1998-06-19 Fri 09:28:19 EDT	sds
;;
;; This file is distributed under the GPL. See
;;	<URL:http://www.gnu.ai.mit.edu/copyleft/gpl.html>
;; for further details.

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

;;; Commentary:

;; Put this file somewhare in your load-path and add the following
;; lines to your .emacs file:

;;   (autoload 'scwm-mode "scwm" "Major mode for editing scwm code. [...]" t)
;;   (setq auto-mode-alist (cons '("scwmrc\\'" . scwm-mode) auto-mode-alist))

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
;; also available: type C-h C-a for apropos and C-h C-s for documentation.
;; Type M-TAB to complete symbol at point.

(eval-and-compile
 (unless (and (fboundp 'cadr) (fboundp 'unless)) (require 'cl))
 (or (fboundp 'apropos-mode) (autoload 'apropos-mode "apropos"))
 (unless (fboundp 'with-output-to-string)
   (defmacro with-output-to-string (&rest body)
     "Execute BODY, return the text it sent to `standard-output', as a string."
     `(let ((standard-output (get-buffer-create (generate-new-buffer-name
                                                 " *string-output*"))))
       ,@body
       (save-excursion
         (set-buffer standard-output)
         (prog1 (buffer-string)
           (kill-buffer standard-output)))))))

;;; Code:

;; user variables
;; ---- ---------

(defvar scwm-repl "scwmrepl" "The path to scwmrepl.")
(defvar scwm-exec "scwmexec" "The path to scwmexec.")

;; Use scheme major mode
(require 'scheme)

;;; XEmacs doesn't have thingatpt. Too bad.
(unless (fboundp 'thing-at-point)
  ;; pacify the compiler (XEmacs only)
  (eval-and-compile (autoload 'id-select-symbol "id-select"))
  (defun thing-at-point (what)
    "Return the thing at point (crippled: symbols only!)."
    (unless (eq what 'symbol)
      (error "crippled `thing-at-point' - symbols only"))
    (let ((zz (id-select-symbol (point))))
      (buffer-substring-no-properties (car zz) (cdr zz)))))

;; user functions
;; ---- ---------

;;;###autoload
(define-derived-mode scwm-mode scheme-mode "Scwm"
  "Major mode for editing scwm code.
Special commands:
\\{scwm-mode-map}
Turning on Scwm mode calls the value of the variable `scwm-mode-hook',
if that value is non-nil.
If you are using Emacs 20.2 or earlier and want to use fontifications,
you have to (require 'font-lock) first.  Sorry.")

(define-key scwm-mode-map [(control j)] 'scwm-eval-print)
(define-key scwm-mode-map [(control c) (control s)] 'scwm-run)
(define-key scwm-mode-map [(control x) (control j)] 'scwm-eval-to-minibuffer)
(define-key scwm-mode-map [(control h) (control s)] 'scwm-documentation)
(define-key scwm-mode-map [(control h) (control a)] 'scwm-apropos)
(define-key scwm-mode-map [(meta tab)] 'scwm-complete-symbol-insert)

;;;###autoload
(defun scwm-run ()
  "Run scwm interaction or pop to an existing one.
Use \\[scheme-send-last-sexp] to eval the last sexp there."
  (interactive)
  (unless (fboundp 'inferior-scheme-mode)
    (let ((ff (symbol-function 'run-scheme)))
      (if (and (consp ff) (eq (car ff) 'autoload)) (load (cadr ff))
	  (error "no `inferior-scheme-mode' and no place to get it from."))))
  (pop-to-buffer (setq scheme-buffer (make-comint "scwm" scwm-repl)))
  (inferior-scheme-mode))

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

;; completion
;; ----------

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
  "Complete the current symbol or SYM by querying scwm using apropos-internal.
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

;; help
;; ----

;;;###autoload
(defun scwm-documentation (pat)
  "Query scwm for documentation for the symbol PAT."
  (interactive (list (scwm-complete-symbol)))
  (with-output-to-temp-buffer "*Help*"
    (princ "SCWM documentation `") (princ pat) (princ "':\n\n")
    (scwm-eval (concat "(procedure-documentation " pat ")")
	       standard-output)))

;;;###autoload
(defun scwm-apropos (pat)
  "List all scwm symbols matching PAT."
  ;; (interactive (interactive-token "SCWM Apropos"))
  (interactive
   (list (read-string
	  "SCWM Apropos: "
	  (format "%s" (or (thing-at-point 'symbol)
			   (progn (skip-syntax-backward "^w")
				  (if (bobp) () (backward-char 1))
				  (thing-at-point 'symbol)) "")))))
  (with-output-to-temp-buffer "*Apropos*"
    (princ "SCWM apropos `") (princ pat) (princ "':\n\n")
    (scwm-eval (concat "(apropos \"" pat "\")") standard-output)
    (set-buffer standard-output) (apropos-mode)))

;; fontifications

(cond ((boundp 'running-xemacs)
       (put 'scwm-mode 'font-lock-defaults
            (get 'scheme-mode 'font-lock-defaults)))
      ((and (string-lessp emacs-version "20.3")
            (boundp 'font-lock-defaults-alist))
       (unless (assq 'scwm-mode font-lock-defaults-alist)
         (setq font-lock-defaults-alist
               (cons (cons 'scwm-mode
                           (cdr (assq 'scheme-mode font-lock-defaults-alist)))
                     font-lock-defaults-alist)))))

(provide 'scwm)
;;; scwm ends here
