;;;; $Id$
;;;; Copyright (C) 1997, 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
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
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm winlist))




(define fvwm-exec-shell "/bin/sh")

(define*-public (fvwm-exec-use-shell #:optional (shell #f))
  "Use SHELL when emulating fvwm \"EXEC\" commands.
Defaults to <envar>$SHELL</envar> or <filename>/bin/sh</filename>."
  (set! fvwm-exec-shell 
	(cond
	 (command => identity)          ; id
	 ((getenv "SHELL") => identity) ; id
	 (else "/bin/sh"))))



(define-public (fvwm-exec command)
  "Run COMMAND as fvwm would.
See also `fvwm-exec-use-shell'."
  (if (eq? 0 (primitive-fork))
      (catch #t (lambda ()
		  (execl
		   fvwm-exec-shell fvwm-exec-shell "-c" 
		   (string-append "exec " command)))
	     (lambda args (primitive-exit 100)))))

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
		   (break)
		   (eval form (current-module)))))
      (close-port read-pipe)
      *unspecified*))))

(define-public (fvwm-nop . args)
  *unspecified*)


;; FvwmM4 compatibility (perhaps should be a separate module)
;; these won't work-- they're placeholders for now
;; guile probably permits us access to a lot of these
;; things, but some new primitives may need to be added
;; --03/10/98 gjb
(define-public TWM_TYPE "scwm")
;;; GJB:FIXME:: new primitives or use guile to implement the below
;(define-public SERVERHOST (X-server-host-name))
;(define-public CLIENTHOST (X-client-host-name))
;(define-public HOSTNAME (process-host-name))
;(define-public OSTYPE (process-ostype))
;(define-public USER (get-user-name (get-uid)))

;;; (X-version-information) should return '(version revision vendor release)
(define X-version-info (X-version-information))

(define-public VERSION (car X-version-info))
(define-public REVISION (cadr X-version-info))
(define-public VENDOR (caddr X-version-info))
(define-public RELEASE (cadddr X-version-info))
(define-public WIDTH display-width)
(define-public HEIGHT display-height)

;;(define (resolution pixels mm) 
;;  (/ (+ (/ (* pixels 100000) mm) 50) 100))

;;; (X-display-information) should return '(x-res y-res planes bits-per-rgb class color)

(define X-display-info (X-display-information))

(define-public X_RESOLUTION (list-ref X-display-info 0))
(define-public Y_RESOLUTION (list-ref X-display-info 1))
(define-public PLANES (list-ref X-display-info 2))  ;; also `display-depth'
(define-public BITS_PER_RGB (list-ref X-display-info 3))
(define-public CLASS (list-ref X-display-info 4))
(define-public COLOR (if (list-ref X-display-info 5) "Yes" "No"))
(define-public SCWM_VERSION (scwm-version))
(define-public FVWM_VERSION "3") ;; for lack of a better number
(define-public OPTIONS "SHAPE XPM")
;(define-public FVWMDIR (default-directory))


;; Perhaps this should be the default behaviour of %x?
(define-public (%x-permit-negative x)
  (if (>= x 0)
      (%x x)
      (x- (%x (- x)))))

;; Perhaps this should be the default behaviour of %y?
(define-public (%y-permit-negative y)
  (if (>= y 0)
      (%y y)
      (y- (%y (- y)))))
