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

(define menu-bg-color (make-color "gray80"))
(define menu-text-color (make-color "black"))
(define menu-stipple-color (make-color "grey60"))
(define menu-font (make-font "fixed"))
(define menu-side-image #f)
(define menu-side-bg-color menu-bg-color)
(define menu-side-bg-color-set #f)
(define menu-bg-image #f)
(define menu-look scwm-menu-look)

;; HACK: GJB:FIXME:: This needs to be done in the root module
;; to replace the primitive in more recent guile snapshots
(defmacro-public reset-hook! (hook)
  `(set! ,hook ()))



(define-module (app scwm base)
  :use-module (app scwm optargs))



(define-public (round/ x y)
  "Reaturn the closest integer to X divided by Y."
  (inexact->exact (round (/ x y))))

(define-public use-scwm-system-proc
;;;**VAR
;;; If #t, `execute' will use `scwm-system' instead of guile's `system'.
;;; This works around a problem observed on pre-glibc Linux 2.0.34 i386
;;; where SIGINT on the controlling tty (i.e., the one that started
;;; scwm) propagates to the child processes.  The end result of the
;;; possible bug is that xterms started by scwm are terminated if the
;;; scwm that started them is terminated using a Ctrl-C to send it a SIGINT.
  #f)

(define-public display-width (car (display-size)))
(define-public display-height (cadr (display-size)))

;; Convenience procedures for specifying positions and sizes.
(define-public (%x x)
  "Return the number of pixels that is X percent of the display width."
  (round/ (* x display-width) 100))

(define-public (%y y)
  "Return the number of pixels that is Y percent of the display height."
  (round/ (* y display-height) 100))

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
  (round/ (* (- 100 x) display-width) 100))

(define-public (%y- y)
  "Return the pixel coordinate Y percent of the height away from the bottom edge."
  (round/ (* (- 100 y) display-height) 100))

(define*-public (w%x x #&optional (w (get-window)))
  "Return a pixel width X percent of the width of window W."
  (round/ (* x (car (window-frame-size w))) 100))

(define*-public (w%y y #&optional (w (get-window)))
  "Return a pixel height Y percent of the height of window W."
  (round/ (* y (cadr (window-frame-size w))) 100))

(define-public (program-exists? program-name)
  "Return #t if PROGRAM-NAME is found as an executable in the current $PATH.
Returns #f otherwise.  See `cached-program-exists?' for a more efficient
version of this."
  (= 0 (system (string-append "which " program-name " >/dev/null" ))))

(define-public (set-menu-foreground! fg)
  "Set the default color for menu text to FG."
  (set! menu-text-color (if (color? fg) fg (make-color fg))))
(define-public (set-menu-background! bg)
  "Set the default background for menus to BG."
  (set! menu-bg-color (if (color? bg) bg (make-color bg)))
  (if (not menu-side-bg-color-set) (set! menu-side-bg-color menu-bg-color)))
(define-public (set-menu-stipple! stipple)
  "Set the default color for stippled (inactive) menu text to STIPPLE."
  (set! menu-stipple-color (if (color? stipple) stipple (make-color stipple))))
(define-public (set-menu-font! font)
  "Set the default font for menu text to FONT."
  (set! menu-font (if (font? font) font (make-font font))))
(define-public (set-menu-side-image! image)
  "Set the default menu side image to IMAGE."
  (set! menu-side-image (if (image? image) image (make-image image))))
(define-public (set-menu-side-background! bg)
  "Set the default background for the menu side image to BG.
If BG is #f, use the default menu background" 
  (cond (bg (set! menu-side-bg-color 
		  (if (color? bg) bg (make-color bg)))
	    (set! menu-side-bg-color-set #t))
	(else (set! menu-side-bg-color menu-bg-color)
	      (set! menu-side-bg-color-set #f))))
(define-public (set-menu-bg-image! image)
  "Set the default menu background image to IMAGE."
  (set! menu-bg-image (if (image? image) image (make-image image))))
(define-public (set-menu-look! look)
  "Set the default menu look to LOOK."
  (if (menulook? look) (set! menu-look look) (error "bad look")))

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
			#&optional (win (get-window)))
  "Move WIN to viewport position X, Y.
If X or Y is #f, then do not move along that axis (use existing
value for that coordinate).
See `move-window' if you wish to move a window to a virtual position."
  (if (not (sticky? win))
      (let ((pos (viewport-position)))
	(if x (set! x (+ x (car pos))))
	(if y (set! y (+ y (cadr pos)))))
      (begin
	(if x (set! x (modulo x display-width)))
	(if y (set! y (modulo y display-height)))))
  (move-window x y win))

;; Give move-to a better name, too
;; GJB:FIXME:DOC: Can this have a doc string?
(define-public move-window-viewport-position move-to)

(define*-public (window-title-height #&optional (win (get-window)))
  "Return WIN's titlebar's height.
See also `window-title-size', `window-title-width'."
  (cadr (window-title-size win)))

(define*-public (window-title-width #&optional (win (get-window)))
  "Return WIN's titlebar's width.
See also `window-title-size', `window-title-height'."
  (car (window-title-size win)))

(define*-public (window-viewport-position #&optional (win (get-window)))
  "Return the position of WIN in pixels within the viewport.
The position is returned as a list of the x coordinate and the y
coordinate in pixels. WIN defaults to the window context in the usual
way if not specified.  See also `window-position'."
  (let ((pos (window-position win)))
    (if (sticky? win) pos
	(apply virtual->viewport pos))))

(define*-public (window-virtual-position #&optional (win (get-window)))
  "Return the virtual position of WIN in pixels.
If WIN is sticky, this returns the position of the window in the
current viewport."
  (let ((pos (window-position win)))
    (if (sticky? win) (apply viewport->virtual pos)
	pos)))

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

;; FIXJTL: if everybody uses this interface, remove the set-menu-*
;; functions, set the variables directly here, and free those names
;; for use for setting a single menu's attributes
;; FIXJTL: image-align - to work like others, C code has to dereference
;; symbols that aren't one of 'top, 'bottom or 'center
(define*-public (menu-style #&key
		     (fg #f) (foreground #f)
		     (bg #f) (background #f)
		     (bg-image #f)
		     (stipple #f) (font #f)
		     (look #f)
		     (side-image #f) (side-bg 'unset))
  "Set various properites for the menus.
See `make-menu' for options on creation of individual menus."
  (if (or fg foreground) (set-menu-foreground! (or fg foreground)))
  (if (or bg background) (set-menu-background! (or bg background)))
  (if stipple (set-menu-stipple! stipple))
  (if font (set-menu-font! font))
  (if look (set-menu-look! look))
  (if side-image (set-menu-side-image! side-image))
  (if (not (eq? side-bg 'unset)) (set-menu-side-background! side-bg))
  (if bg-image (set-menu-bg-image! bg-image)))

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
	   ;; If the last character in the label is an &, leave it
	   ;; as a literal &
	   (if (and (equal? (car char-list) #\&) (not (null? (cdr char-list))))
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
		      (image-side 'menu-side-image)
		      (image-align 'top)
		      (color-bg-image-side 'menu-side-bg-color)
		      (image-bg 'menu-bg-image)
		      (color-text 'menu-text-color)
		      (color-bg 'menu-bg-color)
		      (color-stipple 'menu-stipple-color)
		      (font 'menu-font)
		      (look 'menu-look)
		      (extra #f))
  "Return a menu object with the given attributes.
LIST-OF-MENUITEMS is a list of menuitem objects (each created with
`make-menuitem' or `menuitem').  IMAGE-SIDE is an image object to be
displayed along the left edge of the menu.  IMAGE-ALIGN determines
whether to align that image to the 'top, 'center or 'bottom of the
menu.  COLOR-BG-IMAGE-SIDE is the background color for that image
object.  COLOR-TEXT is a color object or string for the foreground
text color of menu items.  COLOR-BG is a color object or string for
the background color for the menu and menu items.  COLOR-STIPPLE is a
color object for stippled (\"grayed\") menu items.  FONT is a font
object for the font of the menu items.  EXTRA is an extra argument
specific to the menu look used for this menu."
  (if (string? image-side)
      (set! image-side (make-image image-side)))
  (if (string? color-bg)
      (set! color-bg (make-color color-bg)))
  (if (string? color-text)
      (set! color-text (make-color color-text)))
  (if (string? color-stipple)
      (set! color-stipple (make-color color-stipple)))
  (if (string? color-bg-image-side)
      (set! color-bg-image-side (make-color color-bg-image-side)))
  (let ((menu (make-menu list-of-menuitems color-bg color-text color-stipple 
			 font 
			 image-side image-align color-bg-image-side image-bg
			 extra)))
    (set-menu-menu-look! menu look)
    menu))

(define-public (popup-menu-from-decoration menu win button-number)
  "Popup MENU from WIN's decoration numbered BUTTON-NUMBER.
This positions the popup menu appropriately."
  (let* ((pos (window-viewport-position win))
	 (x-ne (car pos))
	 (y (+ (cadr pos) (+ (window-frame-border-width win)
			     (window-title-height win))))
	 (x (if (odd? button-number) 
		(+ x-ne 
		   (window-frame-border-width win)
		   (* (/ (- button-number 1) 2) 
			   (window-title-height win)))
		(- 
		 (+ x-ne (car (window-frame-size)))
		 (window-frame-border-width win)
		 (* (/ button-number 2) 
		    (window-title-height win))))))
    (popup-menu menu #f x y #t)))

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


(define-public (exe command)
  "Return a procedure that, when invoked, executes COMMAND in the background."
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

(defmacro-public thunk (proc)
  `(lambda args (apply ,proc args)))

;; Only define if not already defined by Guile
(if (not (defined? 'remove-hook!))
    (defmacro-public remove-hook! (hook proc)
      `(if (memq ,proc ,hook)
           (set! ,hook
                 (delq! ,proc ,hook)))))


;; add-hook! and remove-hook! are defined in guile's boot-9.scm
;; we still need a reset-hook! though, but only if HAVE_SCM_MAKE_HOOK is 1
;; (otherwise, in post guile-1.3, it's already a primitive)
;; GJB:FIXME:: we can use this code once we support new-style hooks
;(if (not (defined? 'reset-hook!))
;    (defmacro-public reset-hook! (hook)
;      `(set! ,hook ())))

(defmacro-public with-window (win . body)
;;;** Bind the window-context to WIN while evaluating BODY.
;;; All `get-window' calls within BODY will return WIN.
  `(let ((old-window-context (window-context)))
     (dynamic-wind
      (lambda () (set-window-context! ,win))
      (lambda () ,@body)
      (lambda () (set-window-context! old-window-context)))))


;; The above with-window has a bug when used w/ interactive-move
;; if broadcast-config-hook is non-empty whereby select-window
;; gets called multiple times w/o explanation.  See tests/with-window.scm
;; This macro can be used instead when you know ,@body won't
;; throw
(defmacro-public with-window-no-wind (win . body)
;;;** Bind the window-context to WIN while evaluating BODY.
;;; All `get-window' calls within BODY will return WIN.
  `(let ((old-window-context (window-context))
	 (answer #f))
     (set-window-context! ,win)
     (set! answer ,@body)
     (set-window-context! old-window-context)
     answer))


(define-public bell beep)

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

(define-public (unset-message-window-position!)
  "Move the message window back to the default screen-center position."
  (apply set-message-window-position!
	 (append (map (lambda (x) (/ x 2)) (display-size)) (list -.5 -.5))))

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

;; GJB:FIXME:: switch to this after testing it
;; From Jim Blandy -- his [better] version of 
;; my scwm-system, above --09/26/98 gjb
(define (background-system command)
  "Run CMD using /bin/sh -c CMD and return the exit status.
The CMD is run synchronously, and Bourne-shell meta characters
are interpreted by /bin/sh.  E.g., to start CMD in the background,
use a trailing \"&\" character.  See also guile's `system', but note
that it may permit signals on the controlling tty to be seen
by children (observed on Linux, Free/NetBSD, but not on Solaris or HP/UX.
This may be a bug (not meeting POSIX.2 specifications).
Returns the child-pid, or #f if the fork fails."
  (let ((child-pid (primitive-fork)))
    (if (zero? child-pid)

	;; Okay, we're the child process.  We need to catch any and
	;; all errors and exit, or else we'll end up with two Guile
	;; repls trying to read from the same terminal.
	(begin
	  (catch #t
		 (lambda ()
		   ;; Put ourselves in our own process group.
		   (setpgid 0 0)   ;; Jim used (getpid) (getpid) for the args
		   ;; Try to execute the user's command.
		   (execl "/bin/sh" "sh" "-c" command))
		 (lambda args #f))
	  ;; If we return from the exec for any reason, it means it failed.
	  (quit 1))
	
	;; Okay, we're the parent process.  Return the child pid, in
	;; case we want to wait for it at some point in the future.
	child-pid)))


(define*-public (select-window #&optional (kill? #f) (release? #f))
  "Select a window interactively, and return the specified window.
Use a special cursor and let the user click to select the window. The
optional arguments KILL? and RELEASE? indicate whether to use the
\"skull and cross-bones\" kill cursor (recommended for destructive
operations like delete-window and destroy-window), and whether to wait
for a mouse release or act immediately on the click. The former is a
place-holder until we have proper cursor support in scwm.
Returns #f if no window was selected."
  (car (select-viewport-position kill? release?)))


(define*-public (select-window-interactively #&optional (msg #f) (message-window #f))
  "Return an interactively-selected window after prompting (optionally) with MSG.
If given, use message window MESSAGE-WINDOW to display the message, otherwise create
a new message window."
  (if msg
      (let ((msgwin (or (and message-window
			     (begin 
			       (message-window-set-message! message-window msg)
			       message-window))
			(make-message-window msg)))
	    (answer #f))
	(message-window-show! msgwin)
	(set! answer (select-window-interactively-no-message))
	(message-window-hide! msgwin)
	answer)
      (select-window-interactively-no-message)))
;; (select-window-interactively "foo")
