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
      (close-port write-pipe)
      (while #t
	     (let ((form (read read-pipe)))
	       (if (eof-object? form)
		   (break #f)
		   (eval form))))
      (close-port read-pipe)
      *unspecified*))))

(define-public (fvwm-nop . args)
  *unspecified*)


;; FvwmM4 compatibility (perhaps should be a separate module)
;; these won't work-- they're placeholders for now
;; guile probably permits us access to a lot of these
;; things, but some new primitives may need to be added
;; --03/10/98 gjb
;(define-public TWM_TYPE "fvwm")
;(define-public SERVERHOST (X-server-host-name))
;(define-public CLIENTHOST (X-client-host-name))
;(define-public HOSTNAME (process-host-name))
;(define-public OSTYPE (process-ostype))
;(define-public USER (get-user-name (get-uid)))
;;; (X-version-information) should return '(version revision vendor release)
;(define-public VERSION (car (X-version-information)))
;(define-public REVISION (cadr (X-version-information)))
;(define-public VENDOR (caddr (X-version-information)))
;(define-public RELEASE (cadddr (X-version-information)))
;(define-public WIDTH (display-width))
;(define-public HEIGHT (display-height))
;;; (display-parameters) should return '(x-res y-res planes bits-per-rgb class color)
;(define-public X_RESOLUTION (car (display-parameters)))
;(define-public Y_RESOLUTION (cadr (display-resolution)))
;(define-public PLANES (caddr (display-resolution)))
;(define-public BITS_PER_RGB (cadddr (display-resolution)))
;(define-public CLASS (caddddr (display-resolution)))
;(define-public COLOR (cadddddr (display-resolution)))
;(define-public SCWM_VERSION (scwm-version)
;(define-public OPTIONS "SHAPE XPM")
;(define-public FVWMDIR (default-directory))
