;;;; $Id$
;;;; Copyright (C) 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
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



(define-module (app scwm file)
  :use-module (ice-9 string-fun)
  :use-module (ice-9 popen))


;;;;
;;;
;;; Miscellaneous file operations. Probably should be in Guile.
;;;

(define-public (filename-is-complete? fname)
  "Return true if FNAME is a fully qualified pathname.
This is considered to be the case if the string FNAME starts with \"/\",
\"./\" or \"../\", following the convention of many Unix programs."
  (and (> (string-length fname) 0)
       (or (char=? (string-ref fname 0) #\/)
	   (and (char=? (string-ref fname 0) #\.)
		(or
		 (and (> (string-length fname 1))
		      (char=? (string-ref fname 1) #\/))
		 (and (> (string-length fname 2))
		      (char=? (string-ref fname 1) #\.)
		      (char=? (string-ref fname 2) #\/)))))))
			 
(define-public (find-file-in-path fname path)
  "Search for file named FNAME in PATH.
FNAME is a string specifying a file; if it is a fully qualified filename,
as indicated by `filename-is-complete?', it is checked for as is. Otherwise,
each element of the list PATH is used as a directory name to check for the
file. If the file is found, the full pathname to it is returned; if not,
#f is returned."
  (if (filename-is-complete? fname)
      (if (file-exists? fname)
	  fname
	  #f)
      (or-map (lambda (prefix)
		(let ((fp (string-append prefix "/" fname)))
		  (if (file-exists? fp)
		      fp
		      #f))) 
	      path)))

(define-public (string-list->string l)
  "Convert L, a list of strings or characters, to a string."
  (apply string-append l))
;; (string-list->string '("123" "456"))

(define-public (path-list->string-with-colons l)
  "Convert L, a list of string directory names, to a single colon-separated string.
Returns that string."
  (define (insert-colons l)
    (cond ((not (pair? l)) '())
	  ((null? (cdr l)) l)
	  (#t
	   (append (list (car l)) '(":") (insert-colons (cdr l))))))
  (string-list->string (insert-colons l)))

;; (use-modules (ice-9 string-fun))
;; (use-modules (app scwm file))
;; (string-with-colons->path-list "this:is:a:test")
;; (string-with-colons->path-list "")
;; (path-list->string-with-colons '())
;; (path-list->string-with-colons '("foo"))
;; (path-list->string-with-colons '("foo" "bar"))
;; (insert-colons '("foo"))
;; (insert-colons '("foo" "bar"))
(define-public (string-with-colons->path-list s)
  "Convert S, a colon-separated directory pathlist, into a list of directory strings.
Returns that list."
  (separate-fields-discarding-char #\: s list))


(define-public (read-until-eof in)
  "Return all the text from input port IN until eof.
IN should be a newline-terminated Ascii input port."
  (let ((l (read-line in))
	(answer ""))
    (while (not (eof-object? l))
	   (set! answer (string-append answer l "\n"))
	   (set! l (read-line in)))
    answer))

(define-public (output-of-system-cmd cmd)
  "Return the output of command shell execution of CMD.
CMD is run synchronously and its output is piped into the return value
of this function, as a string."
  (let* ((p (open-input-pipe cmd))
	 (answer (read-until-eof p)))
    (close-pipe p)
    answer))

(define-public (first-line-output-of-system-cmd cmd)
  "Return the first line of output of command shell execution of CMD.
CMD is run synchronously and its output is piped into the return value
of this function, as a string.  See also `output-of-system-cmd'
if you want to read all of the output of CMD."
  (let* ((p (open-input-pipe cmd))
	 (answer (read-line p)))
    (close-pipe p)
    answer))

(define-public (execute-with-pidprop command)
  "Execute COMMAND in the background and permit use of `window-pid' on its windows.
Returns the PID of COMMAND."
  (let ((pidprop-so (string-append (scwm-path-exec-prefix) "/bin/scwm_set_pid_property.so")))
    (string->number
     (sans-final-newline
      (first-line-output-of-system-cmd
       (string-append "LD_PRELOAD=" pidprop-so " " command " & echo $!"))))))
;; (use-scwm-modules file xprop-extras stringops (ice-9 string-fun))
;; (use-scwm-modules foo)
;; (string->number (sans-final-newline (first-line-output-of-system-cmd "sleep 3 & echo $!")))
;; (execute-with-pidprop "xeyes")
;; (window-pid (get-window))

