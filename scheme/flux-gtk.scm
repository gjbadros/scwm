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
  :use-module (app scwm wininfo)
  :use-module (app scwm xprop-extras)
  :use-module (app scwm winlist)
  :use-module (app scwm winlist-menu)
  :use-module (app scwm time-convert)
  :use-module (app scwm prompt-string)
  :use-module (app scwm winops)
  :use-module (app scwm path-cache)
  :use-module (app scwm optargs))

(define*-public (rename-window-interactively #&optional (win (get-window)))
  "Prompt for a new name for WIN and change its title.
WIN defaults as usual to the current window context."
  (interactive)
  (prompt-string (string-append "Rename window to: ") 
		 (lambda (new-name)
		   (set-window-title! win new-name))
		 #:initval (window-title win)
		 "Rename-window"))


(define*-public (show-window-list-matching-interactively)
  "Prompt for a wildcard, and popup a list of matching windows (by title)."
  (interactive)
  (prompt-string "Window wildcard? "
		 (lambda (wildcard)
		   (add-timer-hook! 200 handle-pending-events)
		   (show-window-list-menu 1 #f #:only (title-match?? wildcard)))))

;; (show-window-list-matching-interactively)
;; (use-scwm-modules prompt-string)
;; (use-scwm-modules time-convert)
