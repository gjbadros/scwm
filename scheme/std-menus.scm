;;;; $Id$
;;;; Copyright (C) 1997-1999 Sam Steingold, Maciej Stachowiak, and Greg J. Badros
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



(define-module (app scwm std-menus)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm defoption)
  :use-module (app scwm menus-extras)
  :use-module (app scwm style)
  :use-module (app scwm themes)
  :use-module (ice-9 regex))

(if (> guile-version 1.3)
    (use-modules (ice-9 popen)))



(define-public (execute-on-selection command)
  "Run COMMAND in the background, with arguments supplied by the X selection."
  (execute (string-append command " '" (X-cut-buffer-string) "'")))

(define-public (exe-on-selection command)
  "Return a procedure that runs COMMAND in the background on the X selection."
  (lambda () (execute-on-selection command)))

(define*-public (make-hosts-menu host-list #&optional (user (user-name)))
  "Create a telnet menu.
To use this, add the following to the menu of your choice:
  (menuitem \"telnet\" #:action (make-hosts-menu '(\"host1\" \"host2\" ...)))
An optional USER argument specifies the user to telnet as.
The element of the list of hosts can be a host (in which case telnet is
used) or a cons of (host . command)."
  (menu (fold-menu-list
         (map (lambda (hh)
                (if (pair? hh)
                    (menuitem (car hh) #:action
                              (run-in-xterm
                               (string-append (cdr hh) " " (car hh))
                               "-n" "telnet_custom"
                               (string-append "-T telnet_custom:_" (car hh))))
                    (menuitem hh #:action
                              (run-in-xterm
                               (string-append "telnet -E -l " user " " hh)
                               (string-append "-T telnet:_" hh) "-n telnet"))))
              host-list))))

(define-public exe-on-selection-editor (exe-on-selection "$EDITOR"))
(define-public exe-on-selection-gv (exe-on-selection "gv"))
(define-public exe-on-selection-xv (exe-on-selection "xv"))
(define-public exe-on-selection-gimp (exe-on-selection "gimp"))
(define-public exe-on-selection-mpeg_play (exe-on-selection "mpeg_play -dither color"))
(define-public exe-on-selection-mpg3 (exe-on-selection "mpg123"))

(define-scwm-group app-associations "Application Associations")

;; GJB:FIXME:: must handle alist:re->string
;;(define-scwm-option *context-map*
;;  `(("\.(txt|pl|c|cc|h)$" "Edit (emacs)"
;;			    #:action ,exe-on-selection-editor)
;;    ("\.ps$" "View (gv)" #:action ,exe-on-selection-gv)
;;    ("\.(gif|jpg)$" "View (ee)" #:action ,exe-on-selection-xv)
;;    ("\.(gif|jpg|xcf)(\.gz)?$" "Edit (gimp)"
;;				 #:action ,exe-on-selection-gimp)
;;    ("\.mpe?g$" "Play (mpeg_play)"
;;		  #:action ,exe-on-selection-mpeg_play)
;;    ("\.mp3$" "Play (mpg123)" #:action ,exe-on-selection-mpg3))
;;  "An alist mapping filename patterns to applicable menu entries.
;;Whenever the car (a regexp) matches a filename, the cdr is used to
;;build a menuitem which is then added to the context menu."
;;  #:type 'alist:re->string
;;  #:group 'app-associations
;;  )

(define-public (make-context-menu)
  "Create a menu of actions applicable to the filename in the X selection.
The selection must contain a single full pathname."
  (let ((file (X-cut-buffer-string)))
    (menu (append
	   (list (menuitem (string-append "... " file))
		 menu-separator)
	   (if (and file (access? file F_OK))
	       (apply append
		      (map (lambda (entry)
			     (if (not (regexp? (car entry)))
				 (set-car! entry
					   (make-regexp (car entry))))
			     (if (and (regexp-exec (car entry) file))
				 (list (apply menuitem (cdr entry)))
				 ()))
			   (scwm-option-get *context-map*)))
	       ())))))


;; contributed by Glenn Trig
(define*-public (menu-window-theme #&optional force?)
  (menu  
   (map 
    (lambda (x) 
      (menuitem x #:action 
		(lambda () (style-one-window (get-window) 
					     #:use-theme (load-cached-theme x force?))))) 
    (theme-names))))


;; contributed by Glenn Trig
(define*-public (menu-global-theme #&optional force?)
  (menu  
   (map 
    (lambda (x) 
      (menuitem x #:action 
		(lambda () 
		  (window-style 
		   "*" #:use-theme (load-cached-theme x force?)))))
    (theme-names))))
