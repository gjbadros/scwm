;;;; $Id$
;;;; Copyright (C) 1998 Maciej Stachowiak
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



(define-module (app scwm themes)
  :use-module (app scwm style-options)
  :use-module (app scwm style)
  :use-module (app scwm optargs)
  :use-module (app scwm file)
  :use-module (app scwm theme-impl))




(define-public theme-path (list (string-append (scwm-path-prefix) "/share/scwm-themes")))


(add-window-style-option #:use-theme 
			 (lambda (condition theme) 
			   (let ((style (theme:window-style theme)))
			     (make-conditional-style condition 
						     #:use-style style)))
			 'splicing)


(define*-public (use-theme theme #&key (for-windows #t) 
			    (for-menus #t) (for-icons #t)
			    (for-background #t) 
			    (for-global-settings #t))
  (let ((theme (if (string? theme) (load-theme theme) theme)))

    (if for-windows
	(window-style #t #:use-theme theme))
  
    ; MS:FIXME:: implement the rest of the options.
  
    (if for-background
	((theme:background-style theme)))))

(define-public (load-theme fname)
  (let ((full-fname (or 
		     (find-file-in-path fname theme-path)
		     (find-file-in-path (string-append fname ".tar") theme-path)
		     (find-file-in-path (string-append fname ".tar.gz") theme-path)
		     (find-file-in-path (string-append fname ".tgz") theme-path))))
    (cond
     ((not full-fname) 
      (error (string-append "Theme file \"" fname "\" not found.")))
     ((or (has-suffix? full-fname ".tgz") (has-suffix? full-fname ".tar.gz"))
      (load-tgz-theme full-fname fname))
     ((has-suffix? full-fname ".tar")
      (load-tar-theme full-fname fname))
     ((file-is-directory? full-fname)
      (load-directory-theme full-fname fname))
     (else
      (error (string-append "Unrecognized theme type for theme \"" fname 
			    "\"."))))))

(define (eval-from-file fname)
  (with-input-from-file fname 
    (lambda ()
      (let loop ((expr (read)) 
		 (last-val (if #f #f)))
	(if (eof-object? expr)
	    last-val
	    (loop (read) (eval last-val)))))))

(define (load-directory-theme file-name theme-name)
  (let* ((old-dir (getcwd))
	 (result
	  (catch #t
		 (lambda ()
		   (chdir file-name)
		   (load (string-append file-name "/theme.scm"))
		   (module-ref (resolve-module 
				`(app scwm theme ,(string->symbol theme-name)))
			       'the-theme))
		 (lambda args     
		   (chdir old-dir)
		   (error (string-append
			   "Unable to load theme file \"" 
			   file-name "\"."))))))
    (chdir old-dir)
    result))
		   

(define (load-tar-theme file-name theme-name)
  (let* ((old-dir (getcwd))
	 (result
	  (catch #t
		 (lambda ()
		   (chdir theme-unpack-dir)
		   (system (string-append "tar xf " file-name))
		   (load-directory-theme 
		    (string-append theme-unpack-dir "/" theme-name) 
		    theme-name))
		 (lambda args 
		   (chdir old-dir)
		   (error (string-append
			   "Unable to load theme file \"" 
			   file-name "\"."))))))
    result))


(define (load-tgz-theme file-name theme-name)
  (let* ((old-dir (getcwd))
	 (theme-tar-file (string-append
			  theme-unpack-dir "/" theme-name ".tar"))
	 (result
	  (catch #t
		 (lambda ()
		   (chdir theme-unpack-dir)
		   (system (string-append "gzip -dc " file-name
					  " > " theme-tar-file))
		   (load-tar-theme theme-tar-file theme-name))
		 (lambda args (error (string-append
				      "Unable to load theme file \"" 
				      file-name "\"."))))))
    (chdir old-dir)
    result))

(define-public theme-unpack-dir
  (let ((tname (tmpnam)))
    (mkdir tname)
    tname))

(add-hook! shutdown-hook 
	   (lambda (restarting?)
	     (system (string-append "rm -rf " theme-unpack-dir))))
