;;; File: <prefs-menu.scm - 1998-03-31 Tue 13:39:41 EST sds@mute.eaglets.com>
;;; Copyright (C) 1998 Sam Shteingold
;;;	$Id$

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA

;; The purpose of this file is to define function prefs-menu, which
;; returns a menu object.  Use it like this:
;; (prefs-menu . #:image-side img #:color-bg-image-side color etc)

(define-module (app scwm prefs-menu)
  :use-module (app scwm base)
  :use-module (app scwm std-menus)
  :use-module (app scwm optargs)
  :use-module (app scwm flux))



(define-public animation-ms-delay 50)

(define-public save-header
  ";; text from here to the EOF is overwritten by save-settings")

(define-public (save-settings)
  (let ((fd (open user-init-file (logior O_RDWR O_CREAT))))
    ;; (logior S_IRWXU S_IRGRP S_IXGRP S_IROTH S_IXOTH)))
    (do ((ll (read-line fd) (read-line fd)))
	((or (eof-object? ll) (equal? ll save-header))
	 (if (eof-object? ll) (write-all fd save-header "\n"))))
    (truncate-file fd (ftell fd))
    (write-all fd "(set-edge-scroll! 0 0)\n(set-opaque-move-size! 0)\n"
	       "(set-desk-size! " (size->str (desk-size) " "))
    (for-each
     (lambda (ll)
       (let ((getter (cadddr ll)) (setter (caddr ll)))
	 (display ")\n(" fd)
	 (if getter () (display "set! " fd))
	 (write setter fd) (display " " fd)
	 (write (if getter ((symbol-binding #f getter))
		    (symbol-binding #f setter)) fd)))
     settable-object-list)
    (display ")\n" fd)
    (close-port fd)))

(define-public (mod-desk-size! dx dy)
  (let* ((sz (desk-size)) (xx (car sz)) (yy (cadr sz)))
    (set-desk-size! (+ xx dx) (+ yy dx))))

(define scroll-menu
  (menu (list (menuitem "Full" #:action (lambda () (set-edge-scroll! 100 100)))
	      (menuitem "50%" #:action (lambda () (set-edge-scroll! 50 50)))
	      (menuitem "20%" #:action (lambda () (set-edge-scroll! 20 20)))
	      (menuitem "5%" #:action (lambda () (set-edge-scroll! 5 5)))
	      (menuitem "1%" #:action (lambda () (set-edge-scroll! 1 1)))
	      (menuitem "Off" #:action (lambda () (set-edge-scroll! 0 0)))
	      menu-separator
	      (menuitem "Help" #:action
			(show-mesg "Set percentage of screen to scroll\\n\
No saving yet!")))))

(define opaque-move-menu
  (menu (list (menuitem "All" #:action (lambda () (set-opaque-move-size! 100)))
	      (menuitem "50%" #:action (lambda () (set-opaque-move-size! 50)))
	      (menuitem "20%" #:action (lambda () (set-opaque-move-size! 20)))
	      (menuitem "Never" #:action
			(lambda () (set-opaque-move-size! 0)))
	      menu-separator
	      (menuitem "Help" #:action
			(show-mesg "Set the max size of an opaquely\\n\
moved window as percentage of the screen.\\n\
No saving yet!")))))

(define desk-size-menu
  (menu (list (menuitem "2x2" #:action (lambda () (set-desk-size! 2 2)))
	      (menuitem "3x3" #:action (lambda () (set-desk-size! 3 3)))
	      (menuitem "x+1" #:action (lambda () (mod-desk-size! 0 1)))
	      (menuitem "x-1" #:action (lambda () (mod-desk-size! -1 0)))
	      (menuitem "y+1" #:action (lambda () (mod-desk-size! 0 1)))
	      (menuitem "y-1" #:action (lambda () (mod-desk-size! -1 0)))
	      menu-separator
	      (menuitem "Help" #:action
			(show-mesg "Set/change the size of the desk.\\n\
Current desk size is " (size->str (desk-size)) ".")))))

;;; FIX this is a bit more complex than this, since we don't want all of
;;; scwm to stall waiting for the response -- the hack we could
;;; currently use is use xprompt and have it run the apropriate
;;; scwm-exec command (i.e., pass the continuation [the
;;; set-shadow-factor!  e.g.] to the ask-string, and have it run
;;; scwm-exec after getting the value from the user
(define-public (ask-string prompt set-fn)
  (message "Cannot ask for `" prompt "' yet! Sorry..."))

(define-public settable-object-list
  ;; this is a list of lists of: ("title" "help" set-fn get-fn)
  ;; load this file, add to this variable whatever you want,
  ;; then call (menu-prefs)
  '(("Shadow Factor" "Shadow Factor -\\n\
the factor that is used by windows with the current decor to generate\\n\
the relief \"shadow\" color for the regular and hilight background."
     set-shadow-factor! shadow-factor)
    ("Menu Shadow Factor" "Menu Shadow Factor -\\n\
the factor that is used by menus to generate\\n\
the relief \"hilight\" color for the regular and hilight background."
     set-menu-shadow-factor! menu-shadow-factor)
    ("Highlight Factor" "Highlight Factor -\\n\
the factor that is used by windows with the current decor to generate\\n\
the relief \"shadow\" color for the regular and hilight background."
     set-hilight-factor! hilight-factor)
    ("Menu Highlight Factor" "Menu Highlight Factor -\\n\
the factor that is used by menus to generate\\n\
the relief \"shadow\" color for the regular and h ilight background."
     set-menu-hilight-factor! menu-hilight-factor)
    ("XTerm Command" "The command used for starting XTerm,\\n\
\(like `xterm' or `rxvt')." xterm-command #f)
    ;; (list "Icon Font" "The font for icons" set-icon-font! icon-font)
    ("Animation Delay" "??" animation-ms-delay #f)))

(define* (settable-object-menuitem title help set-fn get-fn)
  (let ((cur (to-string (if get-fn ((symbol-binding #f get-fn))
			    (symbol-binding #f set-fn))))
	(stt (if get-fn set-fn (lambda (zz) (symbol-set! #f set-fn zz)))))
    (menuitem title #:action
	      (menu (list (menuitem "Set" #:action
				    (lambda ()
				      (ask-string
				       (string-append "New value for "
						      title " (" cur "):")
				       stt)))
			  menu-separator
			  (menuitem
			   "Help" #:action
			   (show-mesg help "\n\nCurrent-value:\t" cur)))))))

(define-public (menu-prefs . opts)
  (apply
   menu (list (menuitem "Preferences" #f) menu-title menu-separator
	      (menuitem "View All Icons" #:action
			(apply string-append "xv "
			       (map (lambda (st) (string-append st "/* "))
				    image-load-path)))
	      (menuitem "Select Font" #:action "xfontsel")
	      (menuitem "View All Fonts" #:action (show-com "xlsfonts"))
	      menu-separator
	      (menuitem "Info on a Window" #:action window-info)
	      (menuitem "Window Properties" #:action (show-com "xprop"))
	      (menuitem "General Info" #:action show-system-info)
	      menu-separator
	      (menuitem "SCWM interaction" #:action
			(run-in-xterm "-e /usr/local/bin/scwmrepl"))
	      (menuitem "Specific parameters" #:action
			(menu (append!
			       (list
				(menuitem "Opaque Move" #:action
					  opaque-move-menu)
				(menuitem "Desk Size" #:action desk-size-menu)
				(menuitem "Scrolling" #:action scroll-menu))
			       (map (lambda (item)
				      (apply settable-object-menuitem item))
				    settable-object-list))))
	      menu-separator
	      (menuitem
	       "X resources" #:action
	       (make-file-menu (string-append HOME "/.Xresources")
			       (menuitem "Reload" #:action
					 "xrdb -merge ${HOME}/.Xresources")))
	      (menuitem
	       "User Init File" #:action
	       (make-file-menu user-init-file
			       (menuitem "Reload" #:action
					 (lambda () (load user-init-file)))))
	      menu-separator
	      (menuitem "Save settings" #:action save-settings))
   opts))
