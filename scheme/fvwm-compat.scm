;;;; 	Copyright (C) 1997 Maciej Stachowiak
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



(define-module (app scwm fvwm-compat)
  :use-module (app scwm optargs)
  :use-module (app scwm winlist))




(define fvwm-exec-shell "/bin/sh")

(define*-public (fvwm-exec-use-shell #&optional (shell #f))
  (set! fvwm-exec-shell 
	(cond
	 (command => id)
	 ((getenv "SHELL") => id)
	 (else "/bin/sh"))))



(define-public (fvwm-exec command)
  (if (eq? 0 (primitive-fork))
      (catch #t (lambda ()
		  (execl
		   fvwm-exec-shell fvwm-exec-shell "-c" 
		   (string-append "exec " command)))
	     (lambda args (primitive-exit 100)))))

(define*-public (fvwm-none thunk #&key (only '()) (except '()))
  (if (null? (list-windows #:only only #:except except))
      (thunk)))

(define-public (fvwm-pipe-read command)
  (let* ((command-pipes (pipe))
	 (read-pipe (car command-pipes))
	 (write-pipe (cdr command-pipes)))
    (cond
     ((eq? 0 (primitive-fork))
      (dup write-pipe 1)
      (execl
       fvwm-exec-shell fvwm-exec-shell "-c" 
       (string-append "exec " command)))
     (else
      (close write-pipe)
      (while #t
	     (let ((form (read read-pipe)))
	       (if (eof-object? form)
		   (break #f)
		   (eval form))))
      (close read-pipe)
      *unspecified*))))

(define-public (fvwm-nop . args)
  *unspecified*)



