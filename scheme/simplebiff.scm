;;;; -*-scwm-*-
;;;; $Id$
;;;; Copyright (C) 1999 Glenn Trigg
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
;;;   Usage:
;;;
;;; (use-modules (app scwm simplebiff))
;;;
;;; (simplebiff)	;; Default mail dir (/var/spool/mail) & username
;;;
;;; (simplebiff #:mail-spool-dir "/var/mail")	;; A different spool dir
;;;
;;; (simplebiff #:username "aligator")		;; A different user
;;;
;;; (simplebiff #:check-interval)	;; A different check interval
;;;					;; (default is 10s)
;;;
;;; Or any combination of the above.
;;;
;;; Current bugs:
;;;   - Will crash if the resultant mail file is non-existant.

(define-module (app scwm simplebiff)
  :use-module (app scwm time-convert)
  :use-module (app scwm optargs))

(define-public mailfile "")
(define-public mail-file-time 0)
(define-public mail-file-size 0)

(define*-public (simplebiff #:key (mail-spool-dir "/var/spool/mail")
			    (username (getenv "USER"))
			    (activate-proc beep)
			    (deactivate-proc noop)
			    (check-interval 10))
  "Run a simple xbiff-like notification system.
MAIL-SPOOL-DIR tells where the mail files live, USERNAME is
the username of the file to watch (defaults to environement
variable \"USER\".  ACTIVATE-PROC is the procedure to apply when the
file grows, DEACTIVATE-PROC is the procedure to apply when the
file changes without growing (by shrinking or staying the same size).
CHECK-INTERVAL is the number of seconds between checks.
Returns an object to pass to `stop-simplebiff' to turn off
the notifier."
  (let* ((mailfile (string-append mail-spool-dir "/" username))
	 (mail-file-time (stat:mtime (stat mailfile)))
	 (mail-file-size (stat:size (stat mailfile))))
    (letrec ((handle #f)
	     (check-mail-file 
	      (lambda ()
		(let* ((newtime (stat:mtime (stat mailfile)))
		       (newsize (stat:size (stat mailfile))))
		  (cond ((and (> newtime mail-file-time)
			      (> newsize mail-file-size))
			 (activate-proc))
			((and (> newtime mail-file-time)
			      (<= newsize mail-file-size))
			 (deactivate-proc)))
		  (set! mail-file-time newtime)
		  (set! mail-file-size newsize)
		  (set! handle (add-timer-hook! 
				(sec->msec check-interval)
				check-mail-file)))))
	     (remove-hook (lambda () 
			    (if handle (remove-timer-hook! handle)))))
      (check-mail-file)
      (lambda ()
	(remove-hook))
      )))

(define-public (stop-simplebiff sb)
  "Call this with the object returned from `simplebiff' to turn off the notifier"
  (sb))
