;;; File: <std-menus.scm - 1998-07-06 Mon 10:43:28 EDT sds@mute.eaglets.com>
;;;; 	Copyright (C) 1998 Sam Steingold and Maciej Stachowiak

;;;	$Id$

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



(define-module (app scwm std-menus)
  :use-module (app scwm base)
  :use-module (app scwm flux)
  :use-module (app scwm optargs)
  :use-module (ice-9 regex))



;;; --------------------------------------
;;; The screen saver and screen lock menus
;;; --------------------------------------

;; Returns a list of mode names queried from the given XLOCK program.
;; Special modes "random", "bomb", and "blank" are not included in this list.
;; Use xlock-query-program to specify what program's output we should
;; read to compute the list of modes, e.g.,
;; (define xlock-query-program "xlock-stdout")
(define (xlock-query-modes xlock)
  (let ((pipe (open-input-pipe (string-append xlock " -help 2>&1"))))
    (do ((line (read-line pipe) (read-line pipe))
         (start-re (make-regexp "where mode is one of:" regexp/icase)))
        ((or (eof-object? line) (regexp-exec start-re line))
         (if (eof-object? line) (display "xlock modes not found\n"))))
    (do ((line (read-line pipe) (read-line pipe)) (match #f) (ml '())
         (mode-re (make-regexp "^[ 	]*([a-zA-Z0-9]+)")))
        ((eof-object? line) (close-pipe pipe)
         (reverse! (delete! "random" (delete! "bomb" (delete! "blank" ml)))))
      (set! match (regexp-exec mode-re line))
      (if match (set! ml (cons (match:substring match 1) ml))))))

(define-public screensaver-modes
  (xlock-query-modes
   (or (and (defined? 'xlock-query-program) xlock-query-program)
       "xlock")))

(define-public xlock-options
  "-nice -19 +mousemotion +timeelapsed -lockdelay 600 -timeout 30")
(define (run-xlock mode lock)	; returns a lambda!
  (exe (string-append "xlock " xlock-options " -mode " mode
                      (if lock "" " -nolock"))))

;;; to use this, add the following to the menu of your choice:
;;;   (menuitem "Screensaver" #:action (make-xlock-menu #f))
;;; or
;;;   (menuitem "Lock Screen" #:action (make-xlock-menu #t))
(define*-public (make-xlock-menu #&optional (lock? #f))
  (menu (append!
	 (list (menuitem "Lock Screen" #f) menu-title menu-separator
	       (menuitem "Random!" #:action (run-xlock "random" lock?))
	       (menuitem "Blank" #:action (run-xlock "blank" lock?))
	       (menuitem "Bomb" #:action (run-xlock "bomb" lock?)))
	 (fold-menu-list
	  (map (lambda (str) (menuitem str #:action (run-xlock str lock?)))
	       screensaver-modes)))))

;;; to use this, add the following to the menu of your choice:
;;; (menuitem "telnet" #:action (menu-hosts '("host1" "host2" ...)))
(define*-public (make-hosts-menu host-list #&optional (user USER))
  (menu (fold-menu-list
         (map (lambda (hh)
               (menuitem hh #:action
                         (run-in-xterm
                          (string-append "-T telnet:" hh " -n telnet "
                                         "-e telnet -E -l " user " " hh))))
             host-list))))

