;;; $Id$
;;; scwm --- functions for editing and running SCWM code under Emacs

;; Copyright (c) 1998 by Sam Steingold <sds@usa.net>

;; File: <scwm.el - 1998-10-27 Tue 15:47:05 EST sds@eho.eaglets.com>
;; Author: Sam Steingold <sds@usa.net>
;; Version: $Revision$
;; Keywords: language lisp scheme scwm

;; LCD Archive Entry:
;; scwm|Sam Steingold|sds@usa.net|
;; Functions for editing and running SCWM code under Emacs|
;; $Date$|$Revision$||

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
 (or (and (fboundp 'cadr) (fboundp 'unless)) (require 'cl))
 ;; these three are dumped with e20.3
 (unless (fboundp 'quit-window) (defalias 'quit-window 'ignore))
 (unless (fboundp 'help-xref-button) (defalias 'help-xref-button 'ignore))
 (unless (fboundp 'help-setup-xref) (defalias 'help-setup-xref 'ignore))
 ;; why aren't these autoloaded by default?
 (unless (fboundp 'Info-find-node) (autoload 'Info-find-node "info"))
 (unless (fboundp 'inferior-scheme-mode)
   (autoload 'inferior-scheme-mode "cmuscheme"))
 (unless (fboundp 'ignore-errors) (autoload 'ignore-errors "cl" nil nil t))
 (unless (fboundp 'compose-mail) (defun compose-mail (to) (mail nil to)))
 (unless (fboundp 'apropos-mode) (autoload 'apropos-mode "apropos")))
(eval-when-compile
 (require 'cl)                  ; for `gensym'
 (defvar Info-history)          ; defined in info.el
 (defvar Info-current-file)     ; defined in info.el
 (defvar font-lock-defaults-alist) ; defined in font-lock.el
 (defvar scheme-buffer)         ; defined in cmuscheme.el
 (defvar inferior-scheme-mode-map) ; defined in cmuscheme.el
 (defvar scwm-mode-map)         ; kill warnings
 (defvar scwm-mode-syntax-table)) ; kill warnings
(eval-when-compile
 ;; cater to the inferior emacs implementations :-)
 (unless (fboundp 'save-current-buffer)
   (defmacro save-current-buffer (&rest body)
     `(save-excursion ,@body)))
 (unless (fboundp 'with-current-buffer)
   (defmacro with-current-buffer (buffer &rest body)
     "Execute the forms in BODY with BUFFER as the current buffer.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
     `(save-current-buffer
       (set-buffer ,buffer)
       ,@body)))
 (unless (fboundp 'with-output-to-string)
   (defmacro with-output-to-string (&rest body)
     "Execute BODY, return the text it sent to `standard-output', as a string."
     `(let ((standard-output (get-buffer-create (generate-new-buffer-name
                                                 " *string-output*"))))
       ,@body
       (save-excursion
         (set-buffer standard-output)
         (prog1 (buffer-string)
           (kill-buffer standard-output))))))
 (unless (fboundp 'with-temp-buffer)
   (defmacro with-temp-buffer (&rest body)
     "Execute BODY, with current buffer being a temporary one."
     `(let ((standard-output (get-buffer-create (generate-new-buffer-name
                                                 " *temp-buffer*"))))
       (save-excursion (set-buffer standard-output)
                       (unwind-protect (progn ,@body)
                         (kill-buffer standard-output))))))
 (unless (fboundp 'with-face)
   (defmacro with-face (face &rest body)
     "Execute body, which prints to `standard-output', then highlight the output."
     (let ((pp (gensym "wf")))
       `(let ((,pp (with-current-buffer standard-output (point))))
         ,@body
         (put-text-property ,pp (with-current-buffer standard-output (point))
          'face ,face standard-output))))))

;;; Code:

;; user variables
;; ---- ---------

(defvar scwm-repl "scwmrepl" "*The path to scwmrepl.")
(defvar scwm-exec "scwmexec" "*The path to scwmexec.")
(defvar scwm-source-path "/usr/src/scwm/" "*The path to the SCWM sources.")

;; thing-at-point-file-name-chars ==>
(defvar scwm-file-name-chars "~/A-Za-z0-9---_.${}#%,:"
  "Characters allowable in filenames.")

;; Use scheme major mode
(require 'scheme)

;;; XEmacs doesn't have thingatpt. Too bad.
(eval-and-compile
 (unless (fboundp 'thing-at-point)
   ;; pacify the compiler (XEmacs only)
   (eval-when-compile
     (autoload 'id-select-symbol "id-select"))
   (defun thing-at-point (what)
     "Return the thing at point (crippled: symbols only!)."
     (unless (eq what 'symbol)
       (error "crippled `thing-at-point' - symbols only"))
     (let ((zz (id-select-symbol (point))))
       (when zz (buffer-substring-no-properties (car zz) (cdr zz)))))))

(defun scwm-symbol-at-point ()
  "Return symbol at point; look back if not found."
  (or (thing-at-point 'symbol)
      (save-excursion (skip-syntax-backward "^w")
                      (unless (bobp) (backward-char 1))
                      (thing-at-point 'symbol)) ""))

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

(define-key scwm-mode-map [(control j)] 'scwm-eval-last)
(define-key scwm-mode-map [(control c) (control s)] 'scwm-run)
(define-key scwm-mode-map [(control c) (control r)] 'scwm-eval-region)
(define-key scwm-mode-map [(control h) (control s)] 'scwm-documentation)
(define-key scwm-mode-map [(control h) (control a)] 'scwm-apropos)
(define-key scwm-mode-map [(control h) (control f)]
  'scwm-goto-guile-procedure-node)
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
  (inferior-scheme-mode)
  (define-key inferior-scheme-mode-map [(control h)]
    (lookup-key scwm-mode-map [(control h)])))

;;; service variables
(defvar scwm-obarray nil "The obarray for scwm completion.")
(defvar scwm-history nil "The input history of SCWM completions.")

(defun scwm-eval (sexp out)
  "Evaluate the SEXP with scwm-exec and print the results to OUT.
All evaluation goes through this procedure."
  (when (string-match "(\\s *\\(define\\|use-modules\\|load\\)" sexp)
    (setq scwm-obarray nil))
  (call-process scwm-exec nil out nil sexp))

(defun scwm-safe-call (func args out)
  "Call FUNC with ARGS and output to OUT, checking existence of FUNC first."
  (scwm-eval (concat "(if (defined? '" func ") (" func " " args ") "
                     "(display \"This Guile version lacks `" func "'.\n\"))")
             out))

(defvar scwm-eval-to-minibuffer nil
  "*The default destination of SCWM output.
If this is nil, the output from `scwm-eval-sexp' is inserted into the
current buffer, otherwise it goes to the minibuffer.")

;;;###autoload
(defun scwm-eval-sexp (sexp mb-p)
  "Eval the SEXP.  Output to the current buffer or minibuffer.
If the prefix argument (the second argument when called from lisp) is
non-nil, the output goes to the minibuffer, otherwise it is inserted in
the current buffer.  When `scwm-eval-to-minibuffer' is non-nil, the
meaning of the second argument is reversed."
  (interactive "sEval in SCWM: \nP")
  (cond ((or (and mb-p scwm-eval-to-minibuffer)
             (not (or mb-p scwm-eval-to-minibuffer)))
         (newline-and-indent)
         (scwm-eval sexp t)
         (newline))
        (t (message "%s" (with-output-to-string
                           (scwm-eval sexp standard-output))))))

;;;###autoload
(defun scwm-eval-last (mb-p)
  "Evaluate the last sexp with `scwm-eval-sexp'."
  (interactive "P")
  (scwm-eval-sexp (buffer-substring-no-properties
                   (point) (save-excursion (backward-sexp) (point))) mb-p))

;;;###autoload
(defun scwm-eval-region (beg end mb-p)
  "Evaluate the region with `scwm-eval-sexp'."
  (interactive "r\nP")
  (scwm-eval-sexp (buffer-substring-no-properties beg end) mb-p))

(defalias 'advertised-xscheme-send-previous-expression
    'scwm-eval-last)

;; completion
;; ----------

(defun scwm-make-obarray ()
  "Create and return an obarray of SCWM symbols."
  ;; (setq scwm-obarray (scwm-make-obarray))
  ;; Can't use `read' because "? " is read as 32.  Another problem is
  ;; that the symbols will be interned in the standard ELisp obarray
  ;; `obarray', not in the obarray `scwm-obarray'.
  (let ((oa (make-vector 131 0)) (pos 2)) ; obarray
    (with-temp-buffer
      ;; make sure `apropos' is present
      (scwm-eval "(use-modules (ice-9 session))" nil)
      (scwm-safe-call "apropos-internal" "\"\"" (current-buffer))
      (unless (= (char-after 1) ?\()
        (error "Not a list: %s" (buffer-string)))
      (goto-char pos)
      (while (re-search-forward "[ ()]" nil t)
        (intern (buffer-substring-no-properties pos (1- (point))) oa)
        (setq pos (point)))
      oa)))

(defun scwm-obarray ()
  "Ensure `scwm-obarray' is initialized."
  (setq scwm-obarray (or scwm-obarray (scwm-make-obarray))))

(defun scwm-complete-symbol (&optional sym)
  "Complete the current symbol or SYM by querying scwm using apropos-internal.
Returns a string which is present in the `scwm-obarray'."
  (when current-prefix-arg (setq scwm-obarray nil))
  (let ((oa (scwm-obarray)))
    ;; require a match only when the obarry is present
    ;; (in case guile lacks `apropos-internal')
    ;; to be removed when the situation stabilizes.
    (completing-read "SCWM symbol: " oa nil oa
                     (or sym (scwm-symbol-at-point)) 'scwm-history)))

;;;###autoload
(defun scwm-complete-symbol-insert ()
  (interactive)
  (let* ((end (point)) (beg (save-excursion (backward-sexp) (point)))
	 (pat (buffer-substring-no-properties beg end))
	 (comp (try-completion pat (scwm-obarray))))
    (cond ((eq comp t) (message "`%s' is complete" pat))
	  ((null comp) (message "Cannot complete `%s'" pat) (ding))
	  ((not (string= comp pat)) (delete-region beg end) (insert comp))
	  (t (message "Making completion list...")
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list (all-completions pat scwm-obarray)))
	     (message "Making completion list...done")))))

;; help
;; ----

(defvar scwm-bug "scwm-discuss@huis-clos.mit.edu"
  "The address to send bug reports on SCWM.")

;;;###autoload
(defun scwm-bug ()
  "Send a bug report about scwm."
  (interactive) (compose-mail scwm-bug)
  (save-excursion
    (search-forward mail-header-separator nil t) (forward-line 1)
    (call-process "uname" nil t nil "-a")
    (scwm-eval "(use-modules (app scwm flux))" nil)
    (let ((pos (point)))
      (scwm-eval "(system-info-string)" t)
      (delete-char -1) (goto-char pos) (delete-char 1))))

;;;###autoload
(defun scwm-documentation (pat)
  "Query scwm for documentation for the symbol PAT."
  (interactive (list (scwm-complete-symbol)))
  (help-setup-xref (list 'scwm-documentation pat) (interactive-p))
  (with-output-to-temp-buffer "*Help*"
    (with-current-buffer "*Help*"
      (princ "SCWM documentation for `")
      (with-face 'highlight (princ pat))
      (princ "':\n\n ")
      (with-face 'highlight (princ "value"))
      (princ ":\n\n ")
      (scwm-eval (concat "(if (defined? '" pat ") " pat
                         " (display \"not defined\"))")
                 standard-output)
      (princ "\n\n ")
      (with-face 'highlight (princ "documentation"))
      (princ ":\n\n")
      (scwm-safe-call "documentation" (concat "\"" pat "\"") standard-output)
      (princ "\n\n ")
      (with-face 'highlight (princ "procedure-documentation"))
      (princ ":\n\n")
      (scwm-eval (concat "(if (defined? 'procedure-documentation) "
                         "(if (procedure? " pat ") (procedure-documentation "
                         pat ") (begin (display " pat ") (display "
                         "\" is not a procedure\n\")))"
                         " (display \"This Guile version lacks "
                         "`procedure-documentation'.\n\"))")
                 standard-output)
      ;; add buttons to the help message
      (let ((st (syntax-table)))
        (set-syntax-table scwm-mode-syntax-table)
        (goto-char 1) (forward-line 8) ; skip the header and value
        (while (looking-at "trying `") (forward-line 1))
        (let ((pt (point)))
          ;; clicking on quoted `symbol' invokes `scwm-documentation'.
          (while (re-search-forward "`\\(\\(\\sw\\|\\s_\\)+\\)'" nil t)
            (help-xref-button 1 #'scwm-documentation (match-string 1)))
          (goto-char pt)
          ;; calling sequence (quote `?')
          (when (re-search-forward (concat "^(" (regexp-quote pat) " .*)$")
                                   nil t)
            (put-text-property (match-beginning 0) (match-end 0)
                               'face 'highlight))
          (goto-char pt)
          ;; function definition in the source
          (while (re-search-forward
                  (concat "^\\[From \\(\\([" scwm-file-name-chars
                          "\\]+\\):\\([0-9]+\\)\\)]$")
                  nil t)
            (help-xref-button 1 (lambda (fl pos)
                                  (let ((ff (concat scwm-source-path fl)))
                                    (unless (file-readable-p ff)
                                      (error "File `%s' not found" ff))
                                    (pop-to-buffer (find-file-noselect ff)))
                                  (goto-line pos))
                              (list (match-string 2)
                                    (string-to-number (match-string 3))))))
        (set-syntax-table st))
      (help-mode) (goto-char 1) (print-help-return-message))))

;;;###autoload
(defun scwm-apropos (pat)
  "List all scwm symbols matching PAT."
  (interactive
   (list (read-string "SCWM Apropos: " (format "%s" (scwm-symbol-at-point)))))
  (with-output-to-temp-buffer "*Apropos*"
    (with-current-buffer "*Apropos*"
      (princ "Click mouse-2 for documentation.\n\nSCWM apropos `")
      (with-face 'highlight (princ pat))
      (princ "':\n\n")
      (scwm-safe-call "apropos" (concat "\"" pat "\"") standard-output)
      (goto-char (point-max))   ; kill `#<unspecified>'
      (delete-region (point) (progn (beginning-of-line) (point)))
      (goto-char 1) (forward-line 4)
      (sort-lines nil (point) (point-max))
      ;; make the symbols clickable
      (let ((props '(action scwm-documentation mouse-face highlight
                     face italic)) p0 p1 p2 str)
        (while (not (eobp))
          (setq p0 (point) p1 (search-forward ": ")
                p2 (1- (re-search-forward "\\s "))
                str (buffer-substring-no-properties p1 p2))
          (add-text-properties p0 p2 (cons 'item (cons str props)))
          (forward-char -1)     ; in case the line just ended
          (forward-line 1)))
      (apropos-mode) (setq truncate-lines t))))

;; info interface
(defvar scwm-info-file-list
  '(("r4rs" . "Index")
    ("guile-ref" . "Procedure Index")
    ("guile-ref" . "Variable Index")
    ("scwm" . "Function Index")
    ("scwm" . "Variable Index"))
  "AssocList of Info files that describe Guile procedures.
An element is of the form (FILE . NODE).")

(defun scwm-find-guile-procedure-nodes (procedure)
  "Return a list of locations documenting PROCEDURE.
The variable `scwm-info-file-list' defines heuristics for which Info
manual to try.  The locations are of the format used in `Info-history',
i.e. (FILENAME NODENAME BUFFERPOS)"
  (let ((where '())
	(cmd-desc (concat "^\\* " (regexp-quote procedure)
			  ":\\s *\\(.*\\)\\.$"))
	(file-list scwm-info-file-list))
    (save-window-excursion
      (save-excursion
        (while file-list
          (let ((file (car (car file-list)))
                (index (cdr (car file-list))))
            (setq file-list (cdr file-list))
            (ignore-errors
              (Info-find-node file index)
              ;; Take the index node off the Info history.
              (setq Info-history (cdr Info-history))
              (goto-char (point-max))
              (while (re-search-backward cmd-desc nil t)
                (setq where (cons (list Info-current-file
                                        (buffer-substring
                                         (match-beginning 1)
                                         (match-end 1))
                                        0)
                                  where))))))))
    (let ((ww (get-buffer-window "*info*" t)))
      (when ww (quit-window nil ww)))
    where))

;;;###autoload
(defun scwm-goto-guile-procedure-node (procedure)
  "Go to the Info node in the Guile manual for procedure PROCEDURE.
The procedure is found by looking up in the Guile Reference manual's
Procedure Index or in another manual found via the variable
`scwm-info-file-list'."
  (interactive (list (scwm-complete-symbol)))
  (let ((where (scwm-find-guile-procedure-nodes procedure)))
    (unless where (error "Couldn't find documentation for %s" procedure))
    (let ((num-matches (length where)))
      ;; Get Info running, and pop to it in another window.
      (save-window-excursion (info))
      (pop-to-buffer "*info*")
      ;; (filename nodename bufferpos)
      (Info-find-node (car (car where)) (car (cdr (car where))))
      (when (> num-matches 1)
        ;; Info-find-node already pushed (car where) onto
        ;; Info-history.  Put the other nodes that were found on
        ;; the history.
        (setq Info-history (nconc (cdr where) Info-history))
        (message "Found %d other entr%s.  Use %s to see %s."
                 (1- num-matches) (if (> num-matches 2) "ies" "y")
                 (substitute-command-keys "\\[Info-last]")
                 (if (> num-matches 2) "them" "it"))))))

;; fontifications
;; --------------
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
