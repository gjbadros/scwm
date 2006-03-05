;;;; $Id$
;;;; Copyright (C) 1998 Sam Steingold and Maciej Stachowiak
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



(define-module (app scwm wmconfig-menu)
  :use-module (app scwm base)
  :use-module (app scwm optargs))



;;; -----------------------------------------------------------------------
;;; Process the /etc/X11/wmconfig/ directory in SCWM. For RedHat5 GNU/Linux.
;;; -----------------------------------------------------------------------

;;; usage: do
;;; (use-module (app scwm wmconfig-menu))
;;; and add to a menu of your choice the following line:
;;; (menuitem "WMConfig" #:action (make-wmconfig-menu "title" "path"))

(define default-wmconfig-dir "/etc/X11/wmconfig/")
(define default-wmconfig-title "System WM Config")

(define (wmc-fix str)		; kill the ampersand in the string
  (do ((ll (string-length str))
       (ix (string-index str #\& 0) (string-index str #\& ix)))
      ((not ix) str) (string-set! str ix #\ )))

(define (wmc-process-file fl)	; return (group . menuitem)
  (let ((fd (open-input-file fl)))
    (do ((wd (read fd) (read fd)) (mi '()) (gr '()) (nm '()))
	((eof-object? wd) (close-input-port fd)
	 (cons gr (if (eq? nm '()) '() (apply menuitem nm mi))))
      (cond ((string=? wd "mini-icon")
	     (let ((img (make-image (string (read fd)))))
	       ;; could resize image here if we so choose --07/02/99 gjb
	       (set! mi (append (list #:image-left img) mi))))
	    ((string=? wd "exec")
	     (set! mi (append (list #:action (exe (wmc-fix (read fd)))) mi)))
	    ((string=? wd "name") (set! nm (read fd)))
	    ((string=? wd "group") (set! gr (string (read fd))))))))

(define (wmc-process-directory wmconfig-dir)
  (display "Processing directory: ") (display wmconfig-dir)
  (cond ((access? wmconfig-dir R_OK)
	 (display ": directory readable...") (newline)
	 (let ((wmcd (opendir wmconfig-dir)) (res '()))
	   (do ((fl (readdir wmcd) (readdir wmcd)))
	       ((eof-object? fl) (closedir wmcd) (newline) res)
	     (display " ") (display fl)
	     (set! fl (string-append wmconfig-dir fl))
	     (cond ((access? fl R_OK)
		    (if (eqv? (stat:type (stat fl)) 'regular)
			(begin
			  (set! fl (wmc-process-file fl))
			  (if (eq? (car fl) '()) '()
			      (let ((ff (assoc (car fl) res)))
				(cond (ff (set-cdr! ff (cons (cdr fl) (cdr ff))))
				      (#t (set-cdr! fl (list (cdr fl)))
					  (set! res (cons fl res))))))
			  (display "+"))))
		   (#t (display "-"))))))
	(#t (display ": directory unreadable!!!") (newline) '())))

(define (wmc-regroup ls)	; regroup the menu items
  (let ((main '()) (sub '()))
    (for-each (lambda (me)
		(if (string-index (car me) #\/) (set! sub (cons me sub))
		    (set! main (cons me main)))) ls)
    (for-each (lambda (me)
		(let* ((pos (string-index (car me) #\/))
		       (ww (substring (car me) 0 pos))
		       (nn (substring (car me) (+ 1 pos) (string-length (car me))))
		       (tt (assoc ww main)))
		  (set! me (menuitem nn #:action (menu (cdr me))))
		  (if tt (set-cdr! tt (cons me (cdr tt)))
		      (set! main (cons (cons ww (list me)) main)))))
	      sub)
    (map (lambda (me) (menuitem (car me) #:action (menu (cdr me)))) main)))

(define*-public (make-wmconfig-menu 
		 #:optional (wmconfig-title default-wmconfig-title)
		 (wmconfig-dir default-wmconfig-dir))
  "Return a menu object for the window-manager configuration menu."
  (menu (append! (list (menu-title wmconfig-title #f) menu-separator)
		 (wmc-regroup (wmc-process-directory wmconfig-dir)))))
