;;;; $Id$
;;;; Copyright (C) 1999 Michael Schmitz <mschmitz@iname.com>
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



;; This module serves setting up menus from directories that represent
;; either Gnome or KDE menus. It contains the following functions of
;; interest:
;;
;; (make-menu-gnome-system #&optional path title)
;;     returns the Gnome System menus (the one you get as top-level
;;     directory in the panel)
;;
;; (make-menu-another-level #&optional path title)
;;     returns the AnotherLevel menus
;;
;; (make-menu-gnome-user #&optional path title)
;;     returns the Gnome user menus
;;
;; (make-menu-kde #&optional path title)
;;     returns the KDE menus

;; TODO
;;   1.  There's no documentation of the structure of the list set up
;;       by readdir-rec. Some other potentially useful (?) functions
;;       are useless as long as it is missing.
;;   2.  Sort menu items alphabetically and evaluate order specified
;;       in the Gnome .order and KDE .directory files.

(define-module (app scwm gnome-panel-menus)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (ice-9 regex))


;; I need the following function since (basename "/a/b/c") from
;; fvwm-module.scm returns "/c" and I really like this module...
(define (basename filename)
  (let ((index (string-rindex filename #\/)))
    (if index
	(substring filename (+ index 1))
	filename)))


(define (create-menuitem elem)
  (cond
   ((eq? (car elem) 'key-item)
    (let* ((list (cadr elem))
	   (icon (and (pair? (assq 'key-icon list)) (cdr (assq 'key-icon list))))
	   (name (and (pair? (assq 'key-name list)) (cdr (assq 'key-name list))))
	   (exec (cdr (assq 'key-exec list)))
	   (image-height (+ 2 (assoc-ref (font-properties *menu-font*) 'height)))
	   (image-width image-height)
	   (image #f))
      (if (not name)
	  ;; sometimes there's no matching 'Name[*]=' entry in a Gnome
	  ;; *.desktop or KDE *.kdelnk file; we use information from the
	  ;; filename in this case
	  (set! name
		(substring (cdr (assq 'key-file list))
			   0
			   (string-index (cdr (assq 'key-file list)) #\.))))
      (if (not (eq? icon #f))
	  (set! image (make-image icon)))
      (if (not (equal? image #f))
	  (menuitem name
		    #:image-left (clone-scaled-image image
						     image-width
						     image-height)
		    #:action (exe exec))
	  (menuitem name
		    #:action (exe exec)))))
   ((string? (car elem))
    (menuitem (car elem)
	      #:submenu (make-submenu (car elem)
				      (cdr elem))))
   ))


(define (make-submenu default-name menu-spec)
  (let* ((name (or (and (pair? (assq 'key-name menu-spec))
			(cdr (assq 'key-name menu-spec)))
		   default-name))
	 (icon (and (pair? (assq 'key-icon menu-spec)) (cdr (assq 'key-icon menu-spec))))
	 (items (cdr (assq 'key-dir-contents menu-spec)))
	 (image-height (+ 2 (assoc-ref (font-properties *menu-font*) 'height)))
	 (image-width image-height))
    (if (> (length items) 0)
	(menu
	 (append (list
		  (if (not (eq? icon #f))
		      (menu-title name
				  #:image-left (clone-scaled-image (make-image icon)
								   image-width
								   image-height))
		      (menu-title name))
		  menu-separator)
		 (map (lambda (elem)
			(create-menuitem elem))
		      items)))
	(menu
	 (list
	  (menuitem (string-append name " (empty)")
		    #:image-left (clone-scaled-image (make-image "gnome-folder.png")
						     image-width
						     image-height)
		    #:action noop))))))


(define* (make-all-submenus structured-list #&optional title)
  (menu
   (append
    (if (bound? title)
	(list (menu-title title)
	      menu-separator)
	'())
    (map
     (lambda (structured-list-elem)
       (if (string? (car structured-list-elem))
	   (menuitem (car structured-list-elem)
		     #:submenu (make-submenu (car structured-list-elem)
					     (cdr structured-list-elem)))
	   (create-menuitem structured-list-elem)))
     structured-list))))


;; the following function reads and evaluates a Gnome *.desktop or KDE
;; *.kdelnk file
(define (process-dot-desktop desktop-file)
  (let ((result '()))
    (if (and (string? desktop-file)
	     (file-exists? desktop-file)
	     (not (file-is-directory? desktop-file))
	     (access? desktop-file R_OK))
	(begin
	  (set! result (acons 'key-file (basename desktop-file) '()))
	  (call-with-input-file
	      desktop-file
	    (lambda (port)
	      (let* ((lang (getenv "LANG"))
		     (patterns (append (list (list 'key-name (make-regexp "^Name=")))
				       (if lang
					   (list (list 'key-name
						       (make-regexp
							(string-append
							 "^Name\\\["
							 (if (string-index lang #\_)
							     (substring lang 0 (string-index lang #\_))
							     (string-append lang "_\[A-Za-z\]*"))
							 "\\\]=")))
						 (list 'key-name (make-regexp (string-append "^Name\\\[" lang "\\\]="))))
					   '())
				       (list (list 'key-exec (make-regexp "^Exec="))
					     (list 'key-icon (make-regexp "^Icon="))
					     (list 'key-icon (make-regexp "^MiniIcon=")))))
		     (match #f)
		     (value #f))
		(do ((line (read-line port) (read-line port)))
		    ((eof-object? line))
		  (for-each
		   (lambda (key-and-pattern)
		     (set! match (regexp-exec (cadr key-and-pattern) line))
		     (if match
			 (begin
			   (set! value (make-shared-substring line (match:end match)))
			   (if (not (equal? value ""))
			       (begin
				 (set! result (assoc-set! result (car key-and-pattern) value))))
			   ))) patterns))
		)))))
    result))


;; the following function reads and evaluates a Gnome .order files
(define (process-order-file-in-dir directory)
  (let ((result '()))
    (if (and (string? directory)
	     (file-exists? directory)
	     (file-is-directory? directory)
	     (access? directory R_OK))
	(begin
	  (let ((order-file (string-append directory "/.order")))
	    (if (and (file-exists? order-file)
		     (not (file-is-directory? order-file))
		     (access? order-file R_OK))
		(begin
		  (call-with-input-file
		      order-file
		    (lambda (port)
		      (do ((line (read-line port) (read-line port)))
			  ((eof-object? line))
			(set! result (append result (list line)))))))))))
    result))


;; the following function reads and evaluates a Gnome and KDE
;; .directory files
(define (process-directory-file-in-dir directory)
  (let ((result '()))
    (if (and (string? directory)
	     (file-exists? directory)
	     (file-is-directory? directory)
	     (access? directory R_OK))
	(begin
	  (let ((directory-file (string-append directory "/.directory")))
	    (if (and (file-exists? directory-file)
		     (not (file-is-directory? directory-file))
		     (access? directory-file R_OK))
		(begin
		  (call-with-input-file
		      directory-file
		    (lambda (port)
		      (let* ((lang (getenv "LANG"))
			     (patterns (append (list (list 'key-name (make-regexp "^Name=")))
					       (if lang
						   (list (list 'key-name
							       (make-regexp
								(string-append
								 "^Name\\\["
								 (if (string-index lang #\_)
								     (substring lang 0 (string-index lang #\_))
								     (string-append lang "_\[A-Za-z\]*"))
								 "\\\]=")))
							 (list 'key-name (make-regexp (string-append "^Name\\\[" lang "\\\]="))))
						   '())
					       (list (list 'key-icon (make-regexp "^Icon=")))))
			     (match #f)
			     (value #f))
			(do ((line (read-line port) (read-line port)))
			    ((eof-object? line))
			  (for-each
			   (lambda (key-and-pattern)
			     (set! match (regexp-exec (cadr key-and-pattern) line))
			     (if match
				 (begin
				   (set! value (make-shared-substring line (match:end match)))
				   (set! result (assoc-set! result (car key-and-pattern) value))
				   ))) patterns))))))))))
    result))


;; the following function recursively reads a directory containing
;; either a Gnome or KDE Menu and builds a list containing all
;; information required to generate a (tree of) menu(s)
(define (readdir-rec directory)
  (let ((result '())
	(desktop-regexp (make-regexp ".*\.(desktop|kdelnk)$")))
    (if (and (string? directory) 
	     (file-exists? directory)
	     (file-is-directory? directory)
	     (access? directory R_OK))
	(set! result
	      (append result
		      (let outer ((path directory)
				  (level ""))
			(let ((dir (opendir path))
			      (inner-res '()))
			  (let inner ((dirent (readdir dir)))
			    (let ((fullpath (and (string? dirent)
						 (string-append path "/" dirent)))
				  (content-dot-directory '())
				  (content-dot-order '()))
			      (if (string? dirent)
				  (begin
				    (if (and
					 (not (or (equal? dirent ".")
						  (equal? dirent ".."))))
					(begin
					  (set! inner-res
						(append inner-res
							(cond
							 ((file-is-directory? fullpath)
							  ;; determine the name of the menu entry
							  ;; and the icon (if any) from the .directory
							  ;; file located in fullpath
							  (set! content-dot-directory (process-directory-file-in-dir fullpath))
							  ;;(set! content-dot-order (process-order-file-in-dir fullpath))
							  (list (append (append (list dirent) content-dot-directory)
									(if (eq? content-dot-order '())
									    '()
									    (list (list 'key-order content-dot-order)))
									(acons
									 'key-dir-contents
									 (outer fullpath (string-append level "\t"))
									 '()))))
							 (#t
							  (cond
							   ((regexp-exec desktop-regexp dirent)
							    (list (list 'key-item (process-dot-desktop fullpath)))
							    )
							   (#t '())))
							 ))))
					#f)
				    (inner (readdir dir)))
				  #f)))
			  (closedir dir)
			  inner-res)))))
    result))


;; the following function returns a menu that is made up by the
;; information of the given directory (which is expected to contain a
;; Gnome or KDE menu)
(define* (make-menu-from-structured-directory directory #&optional title)
  (let ((structured-dir-list (readdir-rec directory)))
    (if structured-dir-list
	(make-all-submenus structured-dir-list title)
	#f)))


;; like make-menu-from-structured-directory, but for a list of
;; directories
(define* (make-menu-from-list-of-structured-directories directory-list
							title
							#&optional title-list)
  (let ((title-list-for-map '()))
    (if (bound? title-list)
	(set! title-list-for-map title-list))
    (do ((title "untitled" "untitled"))
	((eq? (length title-list-for-map) (length directory-list)))
      (set! title-list-for-map
	    (append title-list-for-map (list title))))
    (menu
     (append
      (list
       (menu-title title)
       menu-separator)
      (map
       (lambda* (directory title)
		(menuitem title
			  #:submenu (make-menu-from-structured-directory directory title)))
       directory-list title-list-for-map)))))


;; returns the Gnome System menus (the one you get as top-level
;; directory in the panel)
(define*-public (make-menu-gnome-system #&optional
					(path "/usr/share/gnome/apps")
					(title "System Menus"))
  (make-menu-from-structured-directory path title)
  )


;; returns the AnotherLevel menus
(define*-public (make-menu-another-level #&optional
					 (path (string-append (getenv "HOME") "/.gnome/apps-redhat"))
					 (title "AnotherLevel Menus"))
  (make-menu-from-structured-directory path title)
  )


;; returns the Gnome user menus
(define*-public (make-menu-gnome-user #&optional
				      (path (string-append (getenv "HOME") "/.gnome/apps"))
				      (title "User Menus"))
  (make-menu-from-structured-directory path title)
  )


;; returns the KDE menus
(define*-public (make-menu-kde #&optional
			       (path "/usr/share/applnk")
			       (title "KDE Menus"))
  (make-menu-from-structured-directory path title)
  )

;;(popup-menu (make-menu-gnome-system-menu))
;;(popup (make-menu-gnome-user-menu))
;;(popup (make-menu-another-level-menu))
;;(popup (make-menu-kde-menu))
;;(popup (make-menu-from-list-of-structured-directories
;;	     (list "/usr/share/gnome/apps" (string-append (getenv "HOME") "/.gnome/apps-redhat") "/usr/share/applnk")
;;	     "TEST"
;;	     (list "System Menus" "AnotherLevel Menus" "KDE Menus")))
