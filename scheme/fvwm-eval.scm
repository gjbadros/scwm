;;;; 	Copyright (C) 1998 Maciej Stachowiak
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



(define-module (app scwm fvwm-eval)
  :use-module (ice-9 string-fun)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm winops))



;;   fvwm-eval
;;
;; This module allows you to evaluate fvwm commands, either from a module
;; or interactively. Interactive usage is probably not terribly useful,
;; however, it would be good to support more of the fvwm commands if other
;; modules need them.

(define display-width (car (display-size)))
(define display-height (cadr (display-size)))

(define (get-two-numeric-args args cont)
  (let* ((s-args (split-after-char 
		  #\space 
		  (sans-leading-whitespace args)
		  (lambda a a))))
    (cont (string->number 
	   (sans-surrounding-whitespace (car s-args)))
	  (string->number 
	   (sans-surrounding-whitespace 
	    (cadr s-args))))))

(define (get-one-numeric-arg args)
  (let* ((s-args (split-after-char 
		  #\space 
		  (sans-leading-whitespace args)
		  (lambda args args))))
    (string->number 
     (sans-surrounding-whitespace (car s-args)))))


(define fvwm-command-hash-table
  (make-hash-table 10))

(define (add-fvwm-command str proc)
  (hash-set! fvwm-command-hash-table (string-downcase!
				      (string-copy str)) proc))

(defmacro define-fvwm-command (str . body)
  `(add-fvwm-command 
    ,str (lambda (args fmod window)
	  ,@body)))

(define-fvwm-command "Desk"
  (get-two-numeric-args
   args (lambda (first second)
	  (set-current-desk! second))))

(define-fvwm-command "Focus"
  (if window
      (focus window)))

(define-fvwm-command "GotoPage"
  (get-two-numeric-args 
   args (lambda (x y)
	  (set-viewport-position! 
	   (* x display-width)
	   (* y display-height)))))

(define-fvwm-command "Iconify"
  (if window
      (let ((arg (get-one-mumeric-arg args)))
	((cond 
	  ((or (not arg) (= arg 0)) toggle-iconify)
	  ((< arg 0) deiconify)
	  (else iconify)) window))))

(define-fvwm-command "Move"
  (if window
      (get-two-numeric-args
       args (lambda (x y)
	      (move-to x y window)))))

(define-fvwm-command "Raise"
  (if window
      (raise-window window)))

(define-fvwm-command "Scroll"
  (get-two-numeric-args args move-viewport))

(define-fvwm-command "Send_ConfigInfo"
    ((caddr fmod)))

(define-fvwm-command "Send_WindowList"
    ((cadddr fmod)))

(define-fvwm-command "Set_mask"
  (set-car! (cdr fmod) (get-one-numeric-arg args)))

(define-fvwm-command "WindowsDesk"
  (move-window-to-desk (first-arg window)))

(define-fvwm-command "KillMe"
   ((list-ref 5 fmod)))

(define*-public (eval-fvwm-command command #&optional (fmod #f) 
				   (window (get-window)))
  (let* ((split-result (split-before-char #\space command 
					  (lambda args args)))
	 (main-cmd (car split-result))
	 (lc-cmd (string-downcase! (string-copy main-cmd)))
	 (args (cadr split-result)))
    ((hash-ref fvwm-command-hash-table lc-cmd) args fmod window)))
