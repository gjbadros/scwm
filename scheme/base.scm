;;;; $Id$
;;;; Copyright (C) 1997-1998 Maciej Stachowiak and Greg J. Badros
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


;;; FIXMS: disgusting hack for now to get these in the root module.

(define menu-bg-color (make-color "gray80"))
(define menu-text-color (make-color "black"))
(define menu-font (make-font "fixed"))



(define-module (app scwm base)
  :use-module (app scwm optargs))



;; Convenience procedures for specifying positions and sizes.
(define-public (%x x)
  "Return the number of pixels that is X percent of the display width."
  (inexact->exact (truncate (/ (* x (car (display-size))) 100))))

(define-public (%y y)
  "Return the number of pixels that is Y percent of the display height."
  (inexact->exact (truncate (/ (* y (cadr (display-size))) 100))))

(define-public (x- x)
  "Return the pixel coordinate that is X pixels left from the right display edge."
  (- (car (display-size)) x))

(define-public (y- y)
  "Return the pixel coordinate that is Y pixels up from the bottom display edge."
  (- (cadr (display-size)) y))

(define-public (%x- x)
  "Return the pixel coordinate X percent of the width away from the right edge."
  (inexact->exact (truncate (/ (* (- 100 x) (car (display-size))) 100))))

(define-public (%y- y)
  "Return the pixel coordinate Y percent of the height away from the bottom edge."
  (inexact->exact (truncate (/ (* (- 100 y) (cadr (display-size))) 100))))

(define*-public (w%x x #&optional (w (get-window)))
  "Return a pixel width X percent of the width of window W."
  (inexact->exact (truncate (/ (* x (car (window-frame-size w))) 100))))

(define*-public (w%y y #&optional (w (get-window)))
  "Return a pixel height Y percent of the height of window W."
  (inexact->exact (truncate (/ (* y (cadr (window-frame-size w))) 100))))

(define-public (execute command) 
  "Execute COMMAND in the background."
  (system (string-append "exec " command " &")))

(define-public (program-exists? program-name)
  "Return #t if PROGRAM-NAME is found as an executable in the current $PATH.
Returns #f otherwise."
  (= 0 (system (string-append "which " program-name " >/dev/null" ))))


;; FIXMS: gross hack alert!
(let ((old-smfg! set-menu-foreground!))
  (set! set-menu-foreground! 
	(lambda (fg) 
	  (old-smfg! fg) 
	  (set! menu-text-color (make-color fg)))))

;; (define-public (set-menu-foreground! fg) (set-menu-colors! fg))
(let ((old-smbg! set-menu-background!))
  (set! set-menu-background!
	(lambda (bg) 
	  (old-smbg! bg)
	  (set! menu-bg-color (make-color bg)))))

;; (define-public (set-menu-background! bg) (set-menu-colors! #f bg))
;; (define-public (set-menu-stipple! st) (set-menu-colors! #f #f st))

;;(define*-public (set-window-foreground! fg #&optional (w (get-window)))
;;  (set-window-colors! fg #f w))

;;(define*-public (set-window-background! bg #&optional (w (get-window))) 
;;  (set-window-colors! #f bg w))

(define*-public (set-window-colors! #&optional (bg #f) (fg #f) (w (get-window)))
  (if bg (set-window-background! bg w))
  (if fg (set-window-foreground! fg w)))

;; relative versions of absolute move procedures.
(define-public (move-pointer x y)
  "Move the X11 pointer X pixels to the right, and Y pixels down.
If X is negative, moves to the left.  If Y is negative moves up."
  (let ((pos (pointer-position)))
    (move-pointer-to (+ x (car pos)) (+ y (cadr pos)))))

(define-public (move-viewport x y)
  "Move the viewport onto the virtual desktop relatively.
Moves X pixels horizontally, to the right if positive, to the left if
negative, and Y pixels vertically, down if positive, up if negative."
  (let ((pos (viewport-position)))
    (set-viewport-position! (+ x (car pos)) (+ y (cadr pos)))))

(define*-public (menu-style #&key 
		     (fg #f) (foreground #f)
		     (bg #f) (background #f)
		     (stipple #f) font mwm mwm-style)
  "Set various properites for the menus.
Many of these are ignored.  See `make-menu' for options on
creation of individual menus."
  (if (or fg foreground) (set-menu-foreground! (or fg foreground)))
  (if (or bg background) (set-menu-background! (or bg background)))
  (if stipple (set-menu-stipple! stipple))
  (if (bound? font)
      (set! menu-font font))
  (if (bound? mwm)
      (set-menu-mwm-style! mwm))
  (if (bound? mwm-style)
      (set-menu-mwm-style! mwm-style)))

;; A subset of the real title-style which is here so people don't have
;; to load all of face.scm to get at it; will probably go away in the
;; future.

(define*-public (simple-title-style #&key font height justify)
  "Set the style for titlebars.
FONT is a font, HEIGHT is a number, JUSTIFY is a legal argument
to `set-title-justify' such as 'left, 'right, or 'center."
  (if (bound? font)
      (set-title-font! font))
  (if (bound? height) 
      (set-title-height! height))
  (if (bound? justify)
      (set-title-justify! justify)))

;; Create an empty menu-item which is drawn as a separator line in menus.
(define-public menu-separator
  (make-menuitem "" #f))

;; menu-title is an alias for menu-separator
(define-public menu-title
  menu-separator)

;; should this be public?
(define (hotkeys-from-name label)
  (let ((char-list (string->list label))
	(return-key-char-list ())
	(return-label-char-list ()))
    (while (not (null? char-list))
	   (if (equal? (car char-list) #\&)
	       (set! return-key-char-list (cons (cadr char-list) 
						return-key-char-list))
	       (set! return-label-char-list (cons (car char-list)
						  return-label-char-list)))
	   (set! char-list (cdr char-list)))
    (list (list->string (reverse return-label-char-list))
	  (list->string (reverse return-key-char-list)))))

(define*-public (menuitem label #&key image-above image-left
			  extra-label action hover-action unhover-action
			  hotkey-prefs)
  "Return a menuitem object with the given attributes.
LABEL is a string for the name on the item.
IMAGE-ABOVE is an image object to show above the label.
IMAGE-LEFT is an image object to show to the left of the label.
EXTRA_LABEL is a second label shown on the item.
ACTION is a menu object or a procedure; if it is a menu object,
the item will popup ACTION as a sub-menu, if it is a procedure,
the procedure will be invoked when the item is selected.
HOVER-ACTION is an procedure to be invoked when the item is
highlighted but not invoked for a moment; UNHOVER-ACTION is 
a procedure to be invoked after the HOVER-ACTION is invoked
when the item is unhighlighted.  HOTKEY-PREFS is a string listing
the characters which are appropriate shortcut-keys for the item;
the first not-yet-used-in-this-menu character will be used for
the shortcut key for the menu item."
  (if (or (bound? hotkey-prefs)
	  (string=? label ""))
      ()
      (let ((result (hotkeys-from-name label)))
	(set! label (car result))
	(set! hotkey-prefs (cadr result))))
  (if (string? image-above)		;; permit "foo.xpm" to mean (make-image "foo.xpm")
      (set! image-above (make-image image-above)))
  (if (string? image-left)
      (set! image-left (make-image image-left)))
  (if (string? action)			;; permit "xterm" to mean (execute "xterm")
      (let ((program-name action))
	(set! action (lambda () (execute program-name)))))
  (make-menuitem label action extra-label image-above image-left
		  hover-action unhover-action hotkey-prefs))


(define*-public (menu list-of-menuitems #&key
		      image-side 
		      (color-bg-image-side 'menu-bg-color)
		      (image-bg #f)
		      (color-text 'menu-text-color)
		      (color-bg 'menu-bg-color)
		      (font 'menu-font))
  "Return a menu object with the given attributes.
LIST-OF-MENUITEMS is a list of menuitem objects (each created with
`make-menuitem' or `menuitem').  IMAGE-SIDE is an image object to be
displayed along the left edge of the menu.  COLOR-BG-IMAGE-SIDE is the
background color for that image object.  COLOR-TEXT is a color object
or string for the foreground text color of menu items.  COLOR-BG is a
color object or string for the background color for the menu and menu
items.  FONT is a font object for the font of the menu items."
  (if (string? image-side)
      (set! image-side (make-image image-side)))
  (if (string? color-bg)
      (set! color-bg (make-color color-bg)))
  (if (string? color-text)
      (set! color-text (make-color color-text)))
  (if (string? color-bg-image-side)
      (set! color-bg-image-side (make-color color-bg-image-side)))
  (make-menu list-of-menuitems image-side color-bg-image-side
	     color-bg color-text image-bg font))

(define-public (image-property image key)
  "Return the KEY property of IMAGE.
See `image-properties' for a list of the keys."
  (cdr (assoc key (image-properties image))))

(define-public (font-property font key)
  "Return the KEY property of FONT.
See `font-properties' for a list of the keys."
  (cdr (assoc key (font-properties font))))

(define-public (color-property color key)
  "Return the KEY property of COLOR.
See `color-properties' for a list of the keys."
  (cdr (assoc key (color-properties color))))

;; for compatability
(define-public load-font make-font)

;;; ----------------------------------------------
;;; General functionality for splitting long menus
;;; ----------------------------------------------
; the max number of lines in a menu
(define-public default-max-fold-lines 30)

(define (split-list ls max)
  (let ((le (length ls)) (tt ()) (t1 ()))
    (cond ((< le max) (list ls))
	  (#t (set! tt (list-tail ls (- max 1))) (set! t1 (cdr tt))
	      (set-cdr! tt ()) (cons ls (split-list t1 max))))))

(define*-public (fold-menu-list 
		ml #&optional (max-lines default-max-fold-lines))
  "Split ML into chained menus of no more than MAX-LINES items.
ML is a list of menuitem objects. MAX-LINES is a number."
  ; split the menu list into groups of max-lines
  (if (<= (length ml) max-lines) ml
      (map (lambda (lm) (menuitem "more..." #:action (menu lm)))
	   (split-list ml max-lines))))

;; Convenience function to return a precedure that will run a system
;; command.
(define-public (exe command) 
  "Return a procedure that runs the system command COMMAND."
  (lambda () (execute command)))

(define-public xterm-command "xterm -e ")

(define-public (run-in-xterm cmd)
  "Return a procedure that runs CMD in an xterm.
Uses the variable \"xterm-command\" to determine how
to run an xterm.  CMD must be simply the name of an
executable (i.e., no options permitted)."
  (exe (string-append xterm-command cmd)))

;; MSFIX:
;; this is redundant w/ below
;; (defmacro-public remove-hook! (var proc)
;;   `(if (memq ,proc ,var)
;;        (set! ,var (delq! proc var))))

;; Only define if not already defined by Guile
(if (not (defined? 'remove-hook!))
	   (defmacro-public remove-hook! (hook proc)
	     `(if (memq ,proc ,hook)
		  (set! ,hook
			(delq! ,proc ,hook)))))

(defmacro-public thunk (proc)
  `(lambda args (apply ,proc args)))


;; add-hook! and remove-hook! are defined in guile's boot-9.scm
;; we still need a reset-hook! though
(defmacro-public reset-hook! (hook)
  `(set! ,hook ()))

(defmacro-public bell ()
  `(beep))

(add-hook! invalid-interaction-hook
	   (lambda () (beep) (display "scwm: invalid interaction\n")))

(add-hook! cannot-grab-hook
	   (lambda () (beep) (display "scwm: cannot grab\n")))


(define-public (set-edge-resistance! s m)
  "Set the edge scroll delay to S, and the edge move threshold to M.
See also `set-edge-scroll-delay!' and `set-edge-move-threshold!'."
  (set-edge-scroll-delay! s)
  (set-edge-move-threshold! m))

(define-public (set-edge-wrap! x y)
  "Set the edge x and y wrap values to X and Y, respectively.
These values should be #t to mean that the pointer should
wrap in the given direction, or #f to not wrap around.
See also `set-edge-x-wrap!' and `set-edge-y-wrap!'."
  (set-edge-x-wrap! x)
  (set-edge-y-wrap! y))

(define-public (set-edge-scroll! x y)
  "Set the edge scroll values to X and Y, respectively.
These values are the number of pixels that the viewport
moves when the pointer hits the edge of the screen.  Use
`%x' and `%y' to convert from a percentage of a screen
dimension to a number of pixels."
  (set-edge-x-scroll! x)
  (set-edge-y-scroll! y))
