;;;; $Id$
;;;; Copyright (C) 1998-1999, Maciej Stachowiak and Greg J. Badros
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
  :use-module (ice-9 string-fun))


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


(define-public (path-list->string-with-colons l)
  "Convert L, a list of string directory names, to a single colon-separated string.
Returns that string."
  (define (insert-colons l)
    (cond ((not (pair? l)) '())
	  ((null? (cdr l)) l)
	  (#t
	   (append (list (car l)) '(":") (insert-colons (cdr l))))))
  (list->string (insert-colons l)))

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
