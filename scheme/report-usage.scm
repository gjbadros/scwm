;;;; $Id$
;;;; Copyright (C) 1998 Maciej Stachowiak and Greg J. Badros
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



(define-module (app scwm report-usage))



;; similar to Greg's log-usage.c which is in turn based on gwm-spy.c
;; from GWM.

(define REPORT_USAGE_PORT 13671)
(define REPORT_USAGE_HOST "uni.cs.washington.edu")

(define USAGE_PACKET_SIZE 1000)
(define USAGE_MAGIC_NUMBER #xfc)

(define (integer/ a b)
  (inexact->exact (truncate (/ a b))))

(define (scwm-version) "post0.8a")
(define (scwm-version-date) 
  "Sat Nov 14 18:09:42 EST 1998 -- $Revision$")

(define-public (report-scwm-usage)
  "Log your usage of scwm to a central host.
The data sent includes the version of scwm you are running and your
hostname. If the environment variable SCWM_DO_NOT_LOG_USAGE is set,
however, no message will be sent even if this function is called."
  (cond 
   ((not (getenv "SCWM_DO_NOT_LOG_USAGE"))
    (let ((sock (socket AF_INET SOCK_DGRAM 0)))
      (connect sock AF_INET (car (hostent:addr-list 
				  (gethostbyname REPORT_USAGE_HOST)))
	       REPORT_USAGE_PORT)
      ;;  (fcntl sock F_SETFL ) turn off O_NDELAY - not sure how in Guile
      (let* ((buf (make-string 1000 #\null))
	     (h (vector-ref (uname) 2))
	     (msg (string-append "XXXscwm " h " : " 
				 (string-append (scwm-version) ", " 
						(scwm-version-date) 
						": STARTED")
				 (string #\null)))
	     (msglen (string-length msg))
	     (msglen2 (min msglen (- 1000 3))))
	;; chop off message at maximum length 1000-3
	(do ((i 0 (+ i 1)))
	    ((> i msglen2))
	  (string-set! buf i (string-ref buf i)))
	(string-set! buf msglen2 (integer->char USAGE_MAGIC_NUMBER))
	(string-set! buf (+ msglen2 1) (integer->char USAGE_MAGIC_NUMBER))
	(string-set! buf (+ msglen2 2) (integer->char USAGE_MAGIC_NUMBER))
	(uniform-array-write buf sock)
	(close sock))))))
