;;; $Id$
;;; flux.scm
;;; Copyright (C) 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
;;;
;;; This are functions used by various sample .scwmrc, but not necessarily
;;; stabilized even as well as the other files in scheme/*.scm
;;; Expect the semantics of these functions to change, and don't
;;; be surprised if some even completely disappear (as we figure out a better
;;; way to do things)



(define-module (app scwm flux-gtk)
  :use-module (ice-9 regex)
  :use-module (app scwm base)
  :use-module (app scwm file)
  :use-module (app scwm netscape)
  :use-module (app scwm wininfo)
  :use-module (app scwm xprop-extras)
  :use-module (app scwm winlist)
  :use-module (app scwm winlist-menu)
  :use-module (app scwm time-convert)
  :use-module (app scwm prompt-string)
  :use-module (app scwm gtk-table-display)
  :use-module (app scwm undo)
  :use-module (app scwm winops)
  :use-module (app scwm register)
  :use-module (app scwm path-cache)
  :use-module (app scwm optargs))

(define*-public (rename-window-interactively #:optional (win (get-window)))
  "Prompt for a new name for WIN and change its title.
WIN defaults as usual to the current window context."
  (interactive)
  (prompt-string (string-append "Rename window to: ") 
		 (lambda (new-name)
		   (set-window-title! win new-name))
		 #:initval (window-title win)
		 "Rename-window"))


(define*-public (show-window-list-matching-interactively)
  "Prompt for a wildcard, and popup a list of matching windows (by title).
Stars are added to either end of the entered string so the default
is a substring search."
  (interactive)
  (prompt-string "Window wildcard? "
		 (lambda (wildcard)
		   (add-timer-hook! 200 X-server-synchronize)
		   (show-window-list-menu 1 #f #:only (title-match?? (string-append "*" wildcard "*"))))))

;; (show-window-list-matching-interactively)
;; (use-scwm-modules prompt-string)
;; (use-scwm-modules time-convert)

(define-public (chop-string str)
;;  (make-shared-substring str 0 (1- (string-length str))))
  (substring str 0 (1- (string-length str))))

;;(output-of-system-cmd "bookmark-grep ")

(define*-public (netscape-bookmark-search)
  "Prompt for a string, and popup a list of matching netscape bookmarks."
  (interactive)
  (prompt-string "Bookmark substring? "
		 (lambda (string)
		   (add-timer-hook! 200 X-server-synchronize)
		   (let ((str (output-of-system-cmd (string-append "bookmark-grep 2>/dev/null " string))))
		     (if (> (string-length str) 1)
			 (gtk-table-from-string (chop-string str)
						#:select-proc (lambda (vals) (netscape-goto-url (car vals)))))))))

(define*-public (gtk-register-info)
  "Display a table containing Scwm registers and their contents"
  (interactive)
  (gtk-table-from-string (register-type-mapping-string)
			 #:titles '("Register" "Contents")
			 #:window-title "Registers"
			 #:select-proc (lambda (str) (insert-undo-global) (jump-to-register (string->symbol (car str))))))
