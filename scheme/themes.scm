;;;; $Id$
;;;; Copyright (C) 1998-1999 Maciej Stachowiak
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
  :use-module (app scwm sort)  ;; GJB:FIXME:G1.3.2: not needed in guile-1.3.2
  :use-module (app scwm defoption)
  :use-module (app scwm optargs)
  :use-module (app scwm file)
  :use-module (app scwm listops)
  :use-module (app scwm theme-impl))



(define-scwm-group file-locations "File locations")

;;;**CONCEPT:Themes
;;; A theme is a named collection of window manager settings.
;;; Themes are still under development, but they are planned to
;;; affect window styles, menus, icons, backgrounds, and various
;;; global settings.

;; this is not primitive either, but need string test case
(define-scwm-option *theme-path* (list (string-append (scwm-path-prefix) "/share/scwm/themes"))
  "The path (list of directory names as strings) to search for theme files."
  #:type 'path
  #:group 'file-locations)

(add-window-style-option #:use-theme 
			 (lambda (condition theme) 
			   (let ((style (theme:window-style theme)))
			     (make-conditional-style condition 
						     #:use-style style)))
			 'splicing)


(define*-public (use-theme theme #:key (for-windows #t) 
			    (for-menus #t) (for-icons #t)
			    (for-background #t) 
			    (for-global-settings #t))
  "Use settings from THEME to set up the window manager.
THEME can be either a theme object (as returned by `load-theme') or a
string naming a theme, in which case that theme will be loaded and
used.  By default, window styles, menus, icons, backgrounds, and
global settings are all affected; if the FOR-WINDOWS, FOR-MENUS,
FOR-ICONS, FOR-BACKGROUND, or FOR-GLOBAL-SETTINGS arguments are #f,
the corresponding areas are not affected.  (Note: at this time,
only windows and backgrounds are affected; the other components
of themes have yet to be implemented.)"
  (let ((theme (if (string? theme) (load-theme theme) theme)))

    (if for-windows
	(window-style #t #:use-theme theme))
  
    ; MS:FIXME:: implement the rest of the options.
  
    (if for-background
	((theme:background-style theme)))))

(define-public (load-theme fname)
  "Returns a theme FNAME which is loaded from `*theme-path*'.
The theme should be either a directory, or a (possibly gzipped)
tar file with extension .tar, .tar.gz, or .tgz."
  (let ((full-fname (or 
		     (find-file-in-path fname *theme-path*)
		     (find-file-in-path (string-append fname ".tar") *theme-path*)
		     (find-file-in-path (string-append fname ".tar.gz") *theme-path*)
		     (find-file-in-path (string-append fname ".tgz") *theme-path*))))
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


(define-public theme-dictionary (make-hash-table 7))

(define*-public (load-cached-theme fname #:optional force?)
  (or (and (not force?) (hash-ref theme-dictionary fname))
      (let ((theme (load-theme fname)))
	(if theme
	    (hash-set! theme-dictionary fname theme)
	    #f)
	theme)))

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
;;	  (catch #t
	  (
		 (lambda ()
		   (chdir file-name)
		   (load (string-append file-name "/theme.scm"))
		   (module-ref (resolve-module 
				`(app scwm theme ,(string->symbol theme-name)))
			       'the-theme)))))
;		 (lambda args     
;		   (chdir old-dir)
;		   (error (string-append
;			   "Unable to load theme file \"" 
;			   file-name "\"."))))))
    (chdir old-dir)
    result))
		   

(define (load-tar-theme file-name theme-name)
  (let* ((old-dir (getcwd))
	 (result
;;	  (catch #t
	  (
		 (lambda ()
		   (chdir theme-unpack-dir)
		   (system (string-append "tar xf " file-name))
		   (load-directory-theme 
		    (string-append theme-unpack-dir "/" theme-name) 
		    theme-name)))))
;		 (lambda args 
;		   (chdir old-dir)
;		   (error (string-append
;			   "Unable to load theme file \"" 
;			   file-name "\"."))))))
    result))


(define (load-tgz-theme file-name theme-name)
  (let* ((old-dir (getcwd))
	 (theme-tar-file (string-append
			  theme-unpack-dir "/" theme-name ".tar"))
	 (result
;;	  (catch #t
	  (
		 (lambda ()
		   (chdir theme-unpack-dir)
		   (system (string-append "gzip -dc " file-name
					  " > " theme-tar-file))
		   (load-tar-theme theme-tar-file theme-name)))))
;		 (lambda args (error (string-append
;				      "Unable to load theme file \"" 
;				      file-name "\"."))))))
    (chdir old-dir)
    result))

(define-public theme-unpack-dir
  (let ((tname (tmpnam)))
    (mkdir tname)
    tname))

(add-hook! shutdown-hook 
	   (lambda (restarting?)
	     (system (string-append "rm -rf " theme-unpack-dir))))


;; contributed by Glenn Trig
(define-public (theme-names) 
  (let ((path-items '())) 
    (for-each 
     (lambda (path) 
       (if (and (string? path) 
		(file-exists? path)
                (file-is-directory? path)
		(access? path R_OK))
           (begin 
             (let ((dir (opendir path)))
	       (let dirloop ((dirent (readdir dir)))
		 (if (string? dirent) 
		     (begin 
		       (let ((fullpath (string-append path "/" 
						      dirent))) 
;;			 (display "Checking ")
;;			 (display fullpath)
;;			 (newline)
			 (if (and 
			      (not (eqv? (string-ref dirent 0) #\.) 
				   ) 
			      (or (and (file-is-directory? fullpath) 
				       (file-exists? (string-append fullpath 
								    "/theme.scm"))) 
				  (or (has-suffix? fullpath ".tar") 
				      (or (has-suffix? fullpath ".tar.gz") 
					  (has-suffix? fullpath ".tgz"))))) 
			     (begin 
			       (set! dirent (basename dirent ".tar")) 
			       (set! dirent (basename dirent ".tar.gz")) 
			       (set! dirent (basename dirent ".tgz")) 
			       (if (not (there-exists? path-items 
						       (lambda (x) (string=? dirent x)))) 
				   (set! path-items 
					 (append path-items 
						 (list dirent))) 
				   #f)) 
			     #f)) 
		       (dirloop (readdir dir))) 
		     #f)) 
	       (closedir dir)) 
	     #f))) 
       *theme-path*) 
     (sort path-items string<?))
    )
