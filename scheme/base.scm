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

(define-public use-scwm-system-proc
;;;**VAR
;;; If #t, `execute' will use `scwm-system' instead of guile's `system'.
;;; This works around a problem observed on pre-glibc Linux 2.0.34 i386
;;; where SIGINT on the controlling tty (i.e., the one that started
;;; scwm) propagates to the child processes.  The end result of the
;;; possible bug is that xterms started by scwm are terminated if the
;;; scwm that started them is terminated using a Ctrl-C to send it a SIGINT.
  #f)



(define-module (app scwm base)
  :use-module (app scwm optargs))



(define-public display-width (car (display-size)))
(define-public display-height (cadr (display-size)))

;; Convenience procedures for specifying positions and sizes.
(define-public (%x x)
  "Return the number of pixels that is X percent of the display width."
  (inexact->exact (truncate (/ (* x display-width) 100))))

(define-public (%y y)
  "Return the number of pixels that is Y percent of the display height."
  (inexact->exact (truncate (/ (* y display-height) 100))))

(define-public (x- x)
  "Return the viewport pixel coordinate X pixels left of the right display edge."
  (- display-width x))

(define-public (y- y)
  "Return the viewport pixel coordinate Y pixels up from the bottom display edge."
  (- display-height y))

(define-public (viewport-x-position)
  "Return the x coordinate of the current viewport."
  (car (viewport-position)))

(define-public (viewport-y-position)
  "Return the y coordinate of the current viewport."
  (cadr (viewport-position)))

(define-public (viewport->virtual x y)
  "Return the virtual coordinates for viewport X,Y."
  (map + (viewport-position) (list x y)))

(define-public (virtual->viewport x y)
  "Return the viewport coordinates for virtual X,Y."
  (map - (list x y) (viewport-position)))

(define-public (virtual-size)
  "Return the size of the virtual screen in pixels."
  (map * (desk-size) (display-size)))

(define-public (vx- x)
  "Return the virtual coordinate X pixels left of the right virtual edge."
  (- (car (virtual-size)) x))

(define-public (vy- y)
  "Return the virtual coordinate Y pixels up from the bottom virtual edge."
  (- (cadr (virtual-size)) y))

(define-public (%x- x)
  "Return the pixel coordinate X percent of the width away from the right edge."
  (inexact->exact (truncate (/ (* (- 100 x) display-width) 100))))

(define-public (%y- y)
  "Return the pixel coordinate Y percent of the height away from the bottom edge."
  (inexact->exact (truncate (/ (* (- 100 y) display-height) 100))))

(define*-public (w%x x #&optional (w (get-window)))
  "Return a pixel width X percent of the width of window W."
  (inexact->exact (truncate (/ (* x (car (window-frame-size w))) 100))))

(define*-public (w%y y #&optional (w (get-window)))
  "Return a pixel height Y percent of the height of window W."
  (inexact->exact (truncate (/ (* y (cadr (window-frame-size w))) 100))))

(define-public (program-exists? program-name)
  "Return #t if PROGRAM-NAME is found as an executable in the current $PATH.
Returns #f otherwise."
  (= 0 (system (string-append "which " program-name " >/dev/null" ))))


;; FIXMS: gross hack alert!
;; use advice macros when written or just clean up in general
(let ((old-smfg! set-menu-foreground!))
  (set! set-menu-foreground!
	(lambda (fg)
	  (old-smfg! fg)
	  (set! menu-text-color (if (color? fg) fg (make-color fg))))))

;; (define-public (set-menu-foreground! fg) (set-menu-colors! fg))
(let ((old-smbg! set-menu-background!))
  (set! set-menu-background!
	(lambda (bg)
	  (old-smbg! bg)
	  (set! menu-bg-color (if (color? bg) bg (make-color bg))))))

;; (define-public (set-menu-background! bg) (set-menu-colors! #f bg))
;; (define-public (set-menu-stipple! st) (set-menu-colors! #f #f st))

;;(define*-public (set-window-foreground! fg #&optional (w (get-window)))
;;  (set-window-colors! fg #f w))

;;(define*-public (set-window-background! bg #&optional (w (get-window)))
;;  (set-window-colors! #f bg w))

(define*-public (set-window-colors! #&optional (bg #f) (fg #f) (win (get-window)))
  "Set WIN's background color to BG, foreground color to FG."
  (if bg (set-window-background! bg win))
  (if fg (set-window-foreground! fg win)))

;; relative versions of absolute move procedures.
(define-public (move-pointer x y)
  "Move the X11 pointer X pixels to the right, and Y pixels down.
If X is negative, moves to the left.  If Y is negative moves up."
  (let ((pos (pointer-position)))
    (move-pointer-to (+ x (car pos)) (+ y (cadr pos)))))

;; Horrible name kept for now for backward compatibility.
;; move-window is the preferred name, and it uses virtual coordinates
(define*-public (move-to x y 
			#&optional (win (get-window))
			(animated? #f)
			(move-pointer-too? #f))
  "Move WIN to viewport position X, Y.
If X or Y is #f, then do not move along that axis (use existing
value for that coordinate).
If ANIMATED? is #t, then animate the window there.
If MOVE-POINTER-TOO? is #t then also move the pointer as the window is moved.
See `move-window' if you wish to move a window to a virtual position."
  (let ((pos (viewport-position)))
    (if x (set! x (+ x (car pos))))
    (if y (set! y (+ y (cadr pos))))
    (move-window x y win animated? move-pointer-too?)))

;; Give move-to a better name, too
;; FIXGJB: Can this have a doc string?
(define-public move-window-viewport-position move-to)

(define*-public (window-title-height #&optional (win (get-window)))
  "Return WIN's titlebar's height.
See also `window-title-size', `window-title-width'."
  (cadr (window-title-size WIN)))

(define*-public (window-title-width #&optional (win (get-window)))
  "Return WIN's titlebar's width.
See also `window-title-size', `window-title-height'."
  (car (window-title-size WIN)))

(define*-public (window-viewport-position #&optional (win (get-window)))
  "Return the position of WIN in pixels within the viewport.
The position is returned as a list of the x coordinate and the y
coordinate in pixels. WIN defaults to the window context in the usual
way if not specified.  See also `window-position'."
  (let ((pos (window-position win)))
    (if (sticky? win) pos
	(apply virtual->viewport pos))))

(define*-public (icon-viewport-position #&optional (win (get-window)))
  "Return the position of WIN's icon in pixels within the viewport.
The position is returned as a list of the x coordinate and the y
coordinate in pixels. WIN defaults to the window context in the usual
way if not specified.  See also `icon-position'."
  (let ((pos (icon-position win)))
    (if (icon-sticky? win) pos
	(apply virtual->viewport pos))))
    
(define-public (move-viewport x y)
  "Move the viewport onto the virtual desktop relatively.
Moves X pixels horizontally, to the right if positive, to the left if
negative, and Y pixels vertically, down if positive, up if negative."
  (let ((pos (viewport-position)))
    (set-viewport-position! (+ x (car pos)) (+ y (cadr pos)))))

(define*-public (menu-style #&key
		     (fg #f) (foreground #f)
		     (bg #f) (background #f)
		     (stipple #f) font)
  "Set various properites for the menus.
Many of these are ignored.  See `make-menu' for options on
creation of individual menus."
  (if (or fg foreground) (set-menu-foreground! (or fg foreground)))
  (if (or bg background) (set-menu-background! (or bg background)))
  (if stipple (set-menu-stipple! stipple))
  (if (bound? font)
      (set! menu-font font)))

;; A subset of the real title-style which is here so people don't have
;; to load all of face.scm to get at it; will probably go away in the
;; future.

(define*-public (simple-title-style #&key font height justify)
  "Set the style for titlebars.
FONT is a font object or a string, HEIGHT is a number of points,
JUSTIFY is a legal argument to `set-title-justify!' such as 'left,
'right, or 'center."
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
		      (image-align 'top)
		      (color-bg-image-side 'menu-bg-color)
		      (image-bg #f)
		      (color-text 'menu-text-color)
		      (color-bg 'menu-bg-color)
		      (font 'menu-font))
  "Return a menu object with the given attributes.
LIST-OF-MENUITEMS is a list of menuitem objects (each created with
`make-menuitem' or `menuitem').  IMAGE-SIDE is an image object to be
displayed along the left edge of the menu.  IMAGE-ALIGN determines
whether to align that image to the 'top, 'center or 'bottom of the
menu.  COLOR-BG-IMAGE-SIDE is the background color for that image
object.  COLOR-TEXT is a color object or string for the foreground
text color of menu items.  COLOR-BG is a color object or string for
the background color for the menu and menu items.  FONT is a font
object for the font of the menu items."
  (if (string? image-side)
      (set! image-side (make-image image-side)))
  (if (string? color-bg)
      (set! color-bg (make-color color-bg)))
  (if (string? color-text)
      (set! color-text (make-color color-text)))
  (if (string? color-bg-image-side)
      (set! color-bg-image-side (make-color color-bg-image-side)))
  (make-menu list-of-menuitems image-side image-align color-bg-image-side
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
;;; the max number of lines in a menu
(define-public default-menu-max-fold-lines 
;;;**VAR
;;; The default number of items that menus are split into by `fold-menu-list'.
  30)

(define (split-list ls max)
  (let ((le (length ls)) (tt ()) (t1 ()))
    (cond ((< le max) (list ls))
	  (#t (set! tt (list-tail ls (- max 1))) (set! t1 (cdr tt))
	      (set-cdr! tt ()) (cons ls (split-list t1 max))))))

(define*-public (fold-menu-list
		ml #&optional (max-lines default-menu-max-fold-lines))
  "Split ML into chained menus of no more than MAX-LINES items.
ML is a list of menuitem objects. MAX-LINES is a number, which
defaults to `default-menu-max-fold-lines'."
  (if (<= (length ml) max-lines) ml
      (map (lambda (lm) (menuitem "more..." #:action (menu lm)))
	   (split-list ml max-lines))))

(define-public (exe command)
  "Return a procedure that runs the system command COMMAND."
  (lambda () (execute command)))

(define-public xterm-command
;;;**VAR
;;; The command to run when a new xterm window is requested.
;;; The string given should refer to a binary or script in the
;;; path and should take a "-e" argument of what to run.
  "xterm")

(define-public remote-shell-command 
;;;**VAR
;;; The command to use to start a remote shell.
;;; It should take a first (non-option) argument of
;;; the hostname to connect to.  "ssh" "rsh" and "telnet"
;;; are each reasonable choices.
  "telnet")

(define-public (run-in-xterm cmd . opts)
  "Return a procedure that runs CMD in an xterm.
Uses the variable `xterm-command' to determine how
to run an xterm.  CMD may include options to the command.
The rest of the arguments are passed as options to the xterm command."
  (exe (string-append xterm-command
                      (apply string-append
                             (map (lambda (st) (string-append " " st)) opts))
                      " -e " cmd)))

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


(defmacro-public scwm-user-var (sym)
  "Lookup sym in the scwm user variable environment.
Currently, this means in the-root-module."
  `(variable-ref (module-variable the-root-module ',sym)))

(define-public (scwm-is-constraint-enabled?)
  "Return #t if scwm has the constraint solver primitives, #f otherwise."
  (bound? scwm-set-master-solver))

(define-public (scwm-system cmd)
  "Run CMD using /bin/sh -c CMD and return the exit status.
The CMD is run synchronously, and Bourne-shell meta characters
are interpreted by /bin/sh.  E.g., to start CMD in the background,
use a trailing \"&\" character.  See also guile's `system', but note
that it may permit signals on the controlling tty to be seen
by children (observed on Linux, Free/NetBSD, but not on Solaris or HP/UX.
This may be a bug (not meeting POSIX.2 specifications)."
  (let ((child-pid (primitive-fork)))
    (cond
     ((< child-pid 0) (error "bad fork"))
     ((> child-pid 0) ;; parent
      (begin
	(cdr (waitpid child-pid))))
     (else ;; child
      (begin
	(setpgid 0 0)
	(execlp "/bin/sh" "sh" "-c" cmd))
      ))))

(define-public (execute command)
  "Execute COMMAND in the background."
  ((if use-scwm-system-proc scwm-system system) (string-append "exec " command " &")))
