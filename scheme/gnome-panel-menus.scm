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
;;
;; (make-menu-from-structured-directory directory #&optional title)
;;     the following function returns a menu that is constructed from
;;     the information in the given directory (see below for a
;;     description of the directories contents)
;;
;; (make-menu-from-list-of-structured-directories directory-list
;;                                                title
;;                                                #&optional title-list)
;;     like make-menu-from-structured-directory, but for a list of
;;     directories
;;
;; (make-all-submenus structured-list #&optional title)
;;     the following function returns a menu which is set up according
;;     to the list given as parameter; The structure of the list is
;;     described below.
;;
;;
;; Structure of a directory that contains a Gnome menu:
;;   1. Subdirectories represent sub-menus.
;;
;;   2. Each directory contains a list of files named
;;      '*.desktop'. These files define ordinary menu-items. The
;;      format of the files - as far as this module is concerned - is
;;      like this:
;;
;;        Name=<default name of the menu-item>
;;        Name[de]=<name of the menu-item in case LANG is "de">
;;        
;;        ... some more language specific lines ...
;;
;;        Exec=<command to be executed>
;;        Icon=<the icon to be displayed to the left of the menu-item>
;;      
;;      All lines but the Exec-line are optional. If there is no
;;      Icon-line, there simply will be no icon. The name of the
;;      menu-item is determined as follows:
;;
;;        1. Take the value of the line Name[$LANG] if there is a line
;;           matching.
;;        2. If $LANG is defined and it contains an underscore (for
;;           example 'en_US') the part heading the '_' is used for
;;           identifying the matching Name-line. So, if LANG=en_US we
;;           look for a line like Name[en].
;;        3. In case there is no line for your language setting (or
;;           LANG is simply undefined), the default name is used (if
;;           any).
;;        4. If there is no line matching 'Name.*=', the base name of
;;           the file itself is taken as the name of the menu-item.
;;
;;      REMARKS:
;;        1. The desktop-files are name '*.kdelnk' instead of
;;           '*.desktop'.
;;
;;   3. All but the top-level directory contain a file named
;;      '.directory'. Just like the desktop-files specify the ordinary
;;      menu-items, the directory-files do for sub-menus. The format
;;      and the algorithm for determining the name of the sub-menu is
;;      almost identical to the one described above. The only
;;      difference is that directory-files do not contain an Exec-line
;;      (well, at least it's ignored).
;;
;;      REMARKS:
;;        1. KDE's directory-files may contain an additional line that
;;           defines the sort order of the menu-items in the
;;           corresponding menu. The format of the line is
;;
;;             SortOrder=<file-or-dir>,<file-or-dir>,<file-or-dir>,...
;;
;;           Look at the description of the '.order' file below for
;;           some more information concerning the sort order of
;;           menu-items.
;;
;;        2. It is no error if the top-level directory contains a
;;           '.directory' file, but it will not be evaluated by the
;;           panel. The same applies to the functions
;;           make-menu-gnome-system, make-menu-another-level,
;;           make-menu-gnome-user and make-menu-kde. However, if you
;;           use the more basic functions
;;           make-menu-from-structured-directory or
;;           make-menu-from-list-of-structured-directories the file is
;;           evaluated if not title has been given as parameter.
;;
;;   4. Each subdirectory may contain a file named '.order'. This file
;;      lists some of the subdirectories (respectively sub-menus) and
;;      desktop-files. The order of the entries in this file defines
;;      the order of the menu-items corresponding to the
;;      subdirectories and desktop-files listed. All menu-items that
;;      belong to a subdirectory or desktop-file not listed in
;;      '.order' are sorted alphabetically (case is ignored). The name
;;      used for sorting the menu-items is the name of the
;;      subdirectory or the name of the menu-item that is take from
;;      the desktop-file (see above).
;;
;;
;; While all interface functions of this module read a directory (or a
;; list of directories) and build a menu from the information
;; extracted, the function make-all-submenus uses the data specified
;; in a special kind of list to produce a menu (actually all other
;; public function build a list and call make-all-submenus). Now, how
;; does this list look like? There are two types of items in these
;; lists:
;;
;;   1. Items that represent ordinary menu-items:
;;      They look like this
;;
;;          '(key-item ((key-icon . "gnome-background.png")
;;                      (key-exec . "background-properties-capplet")
;;                      (key-name . "Background")
;;                      (key-file . "background-properties.desktop")))
;;
;;      Not all of the entries in the alist are mandatory. You don't
;;      need to define an icon, but 'key-exec' must be present (the
;;      value defines the command line). Either 'key-name' or
;;      'key-file' have to be there, they define the name of the
;;      menu-item. If you provide both of them, 'key-name' will be
;;      used.
;;
;;   2. Items that define a sub-menu:
;;      An item that defines a sub-menu might look like this one:
;;
;;          '("Desktop"
;;            (key-name . "Desktop")
;;            (key-order ("theme-selector.desktop" "foo.desktop" "bar"))
;;            (key-dir-contents
;;              (key-item ((key-exec . "theme-selector-capplet")
;;                         (key-name . "Theme Selector")
;;                         (key-file . "theme-selector.desktop")))))
;;
;;       The car of a list specifying a sub-menu has to be a string (a
;;       silly design decision, maybe I'll change this, someday). It
;;       is used as default name of the sub-menu. 'key-name' is an
;;       optional entry that overrides this default name. Another
;;       optional entry 'key-order' specifies the sort order of the
;;       menu-items in the directory to be generated. If no sort order
;;       is specified, all entries will be sorted alphabetically (case
;;       is ignored). The list of strings that define the sort order
;;       contains the names of desktop-files (ending with .desktop
;;       (gnome) or .kdelnk (KDE)) and directories. Entries that don't
;;       name an existing file or directory are ignored. The list of
;;       menu-items itself is defined by the value of
;;       'key-dir-contents'. If the value is #f, there will be a
;;       single menu-item telling the user that this is an empty menu.
;;
;;
;; TODO
;;   1.  Some applications are terminal applications, i.e. they need
;;       to be run in a xterm (or something like that). Whether or not
;;       the application requires a terminal is specified in the
;;       desktop-file. This information is not evaluated currently.


(define-module (app scwm gnome-panel-menus)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm sort)
  :use-module (ice-9 regex))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; useful utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A tiny function that splits a string and returns a list of
;; items. The argument 'separators' is a string containing separator
;; characters used for splitting the string 'string'.
(define (split separators string)
  (if (not (and (string? separators) (string? string)))
      #f)
  (let* ((result '())
	 (regexp (make-regexp (string-append "([^"
					     separators
					     "]*)(["
					     separators
					     "])?(.*)?")))
	 (match (regexp-exec regexp string)))
    (do ((item (match:substring match 1) (match:substring match 1))
	 (rest (match:substring match 3) (match:substring match 3)))
	((equal? item ""))
      (set! match (regexp-exec regexp rest))
      (set! result (append result (list item))))
    result))


(define (get-name-of-menuitem item)
  (if (string? (car item))
      (car item)
      (let ((name (and (pair? (assq 'key-name item)) (cdr (assq 'key-name item)))))
	(if (not name)
	    ;; sometimes there's no matching 'Name[*]=' entry in a Gnome
	    ;; *.desktop or KDE *.kdelnk file; we use information from the
	    ;; filename in this case
	    (and (pair? (assq 'key-file item))
		 (substring (cdr (assq 'key-file item))
			    0
			    (string-index (cdr (assq 'key-file item)) #\.)))
	    name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; some functions reading and processing Gnome and KDE menu directories
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
			     (sort-order-pattern (make-regexp "^SortOrder=")) ;; the sort order of the menuitems is
			                                                      ;; specified in the .directory files for KDE
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
				   ))) patterns)
			  (set! match (regexp-exec sort-order-pattern line))
			  (if match
			      (let ((order-spec (split "," (match:suffix match))))
				(set! result (append result (list (list 'key-order order-spec))))))
			  )))))))))
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
							  (set! content-dot-order (process-order-file-in-dir fullpath))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; some functions used for sorting menu-items
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sort-items order-spec items)
  (let ((items-in-order-spec '())
	(remaining-items '()))
    (if order-spec
	(begin
	  ;; split the items into two lists, one for the items to be
	  ;; sorted according to the order specification, another one
	  ;; which is sorted alphabetically
	  (for-each (lambda (elem)
		      (if (member (or (and (string? (car elem)) (car elem))
				      (cdr (assq 'key-file (cadr elem)))) order-spec)
			  (set! items-in-order-spec (append items-in-order-spec (list elem)))
			  (set! remaining-items (append remaining-items (list elem)))))
		    items))
	(set! remaining-items items))
    (append (sort-according-to-order-spec order-spec items-in-order-spec)
	    (sort-alphabetically remaining-items))))


(define (sort-alphabetically items)
  (sort items (lambda (a b)
		(string<? (string-downcase (get-name-of-menuitem (or (and (string? (car a)) a) (cadr a))))
			  (string-downcase (get-name-of-menuitem (or (and (string? (car b)) b) (cadr b))))))))


(define (sort-according-to-order-spec order-spec items)
  (if order-spec
      (sort items (lambda (a b)
		    (> (length (member (or (and (string? (car a)) (car a))
					   (cdr (assq 'key-file (cadr a)))) order-spec))
		       (length (member (or (and (string? (car b)) (car b))
					   (cdr (assq 'key-file (cadr b)))) order-spec)))))
      '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; functions creating menu-items or entire menus
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-menuitem elem)
  (cond
   ((eq? (car elem) 'key-item)
    (let* ((list (cadr elem))
	   (icon (and (pair? (assq 'key-icon list)) (cdr (assq 'key-icon list))))
	   (name (get-name-of-menuitem list))
	   (exec (cdr (assq 'key-exec list)))
	   (image-height (+ 2 (assoc-ref (font-properties *menu-font*) 'height)))
	   (image-width image-height)
	   (image #f))
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
  (let* ((name (or (get-name-of-menuitem menu-spec)
		   default-name))
	 (icon (and (pair? (assq 'key-icon menu-spec)) (cdr (assq 'key-icon menu-spec))))
	 (order-spec (and (pair? (assq 'key-order menu-spec)) (cadr (assq 'key-order menu-spec))))
	 (items '())
	 (image-height (+ 2 (assoc-ref (font-properties *menu-font*) 'height)))
	 (image-width image-height))
    (if (> (length (cdr (assq 'key-dir-contents menu-spec))) 0)
	(begin
	  ;; before setting up the menu, we have to determine the
	  ;; order the menu-items should appear in
	  (set! items (sort-items order-spec (cdr (assq 'key-dir-contents menu-spec))))
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
			items))))
	(menu
	 (list
	  (menuitem (string-append name " (empty)")
	   #:image-left (clone-scaled-image (make-image "gnome-folder.png")
					    image-width
					    image-height)
	   #:action noop))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; public functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the following function returns a menu which is set up according to
;; the list given as parameter; The structure of the list is described
;; in this modules header.
(define*-public (make-all-submenus structured-list #&optional title)
  (menu
   (append
    (if (bound? title)
	(list (menu-title title)
	      menu-separator)
	'())
    (let ((order-spec (and (pair? (assq 'key-order structured-list))
			   (cadr (assq 'key-order structured-list)))))
      (map
       (lambda (structured-list-elem)
	 (if (string? (car structured-list-elem))
	     (menuitem (car structured-list-elem)
		       #:submenu (make-submenu (car structured-list-elem)
					       (cdr structured-list-elem)))
	     (create-menuitem structured-list-elem)))
       (sort-items order-spec structured-list))))))


;; the following function returns a menu that is constructed from the
;; information in the given directory (see above for a description of
;; the directories contents)
(define*-public (make-menu-from-structured-directory directory #&optional title)
  (let ((structured-dir-list (readdir-rec directory))
	(content-dot-directory #f))
    (if structured-dir-list
	(begin
	  (if (not (bound? title))
	      (get-name-of-menuitem (process-directory-file-in-dir directory)))
	  (make-all-submenus structured-dir-list title))
	#f)))


;; like make-menu-from-structured-directory, but for a list of
;; directories
(define*-public (make-menu-from-list-of-structured-directories directory-list
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
  (make-menu-from-structured-directory path title))


;; returns the AnotherLevel menus
(define*-public (make-menu-another-level #&optional
					 (path (string-append (getenv "HOME") "/.gnome/apps-redhat"))
					 (title "AnotherLevel Menus"))
  (make-menu-from-structured-directory path title))


;; returns the Gnome user menus
(define*-public (make-menu-gnome-user #&optional
				      (path (string-append (getenv "HOME") "/.gnome/apps"))
				      (title "User Menus"))
  (make-menu-from-structured-directory path title))


;; returns the KDE menus
(define*-public (make-menu-kde #&optional
			       (path "/usr/share/applnk")
			       (title "KDE Menus"))
  (make-menu-from-structured-directory path title))



;;(get-name-of-menuitem '("Desktop" (key-name . "Desktop")))
;;(get-name-of-menuitem '((key-name . "URL Handlers") (key-file . "url-properties.desktop")))
;;(get-name-of-menuitem '((key-file . "url-properties.desktop")))
;;(get-name-of-menuitem '((key-f . "url-properties.desktop")))

;;(sort-alphabetically '(("Peripherals" (key-name . "Peripherals")) (key-item ((key-name . "URL Handlers") (key-file . "url-properties.desktop"))) (key-item ((key-name . "Menu editor") (key-file . "gmenu.desktop")))))

;;(sort-according-to-order-spec '(key-order ("gnomecc.desktop" "Desktop" "Multimedia" "Imlib_config.desktop")) '(("Desktop" (key-name . "Desktop")) ("Multimedia" (key-name . "Multimedia")) (key-item ((key-name . "GNOME Control Center") (key-file . "gnomecc.desktop"))) (key-item ((key-name . "Imlib Configuration Options") (key-file . "Imlib_config.desktop")))))

;;(readdir-rec "/usr/share/gnome/apps")
;;(popup-menu (make-menu-gnome-system))
;;(popup-menu (make-menu-gnome-user))
;;(popup-menu (make-menu-another-level))
;;(popup-menu (make-menu-kde))
;;(popup-menu (make-menu-from-structured-directory (string-append (getenv "HOME") "/.gnome/apps-redhat")))
;;(popup (make-menu-from-list-of-structured-directories
;;	     (list "/usr/share/gnome/apps" (string-append (getenv "HOME") "/.gnome/apps-redhat") "/usr/share/applnk")
;;	     "TEST"
;;	     (list "System Menus" "AnotherLevel Menus" "KDE Menus")))
