;;;; $Id$
;;;; Copyright (C) 1997-1999 Greg J. Badros and Maciej Stachowiak
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
(define menu-title-font (make-font "fixed"))
(define menu-side-image #f)
(define menu-side-bg-color menu-bg-color)
(define menu-side-bg-color-set #f)
(define menu-bg-image #f)
(define menu-look scwm-menu-look)
(define menu-hl-fg-color #f) ;; (make-color "yellow"))
(define menu-hl-bg-color #f) ;; (make-color "black"))



(define-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm defoption)
  )



(define-scwm-group system "System")
(define-scwm-group menu "Menu")

(if (bound? hash-fold)
    (define-public (hash-table->alist table)
      (hash-fold acons () table))
    (define-public (hash-table->alist h) 
      (apply append (vector->list h))))

(define-public (add-hook-once! hook proc)
  "Add PROC to HOOK only if it does not contain PROC already."
  (if (not (memq proc (hook->list hook)))
      (add-hook! hook proc)))

(define-public (append-hook-once! hook proc)
  "Append PROC to HOOK only if it does not contain PROC already."
  (if (not (memq proc (hook->list hook)))
      (append-hook! hook proc)))

(define-public (round/ x y)
  "Return the closest integer to X divided by Y."
  (inexact->exact (round (/ x y))))

(define-public (half x)
  "Return the closest integer to half of X."
  (quotient x 2))

(define-public (sleep-ms ms)
  "Delay for MS milliseconds.
Note that timer-hooks are much more useful in nearly all
cases.  See `add-timer-hook!'."
  (select '() '() '() 0 (* 1000 ms)))

(define-public (maybe-make-color obj)
  "Try to make OBJ into a color and return that color object.
Returns #f if OBJ is not a color object or a string."
  (catch #t
	 (lambda ()
	   (cond
	    ((color? obj) obj)
	    ((string? obj) (if (or (string=? "inherit" obj) 
				   (string=? "#f" obj)) #f (make-color obj)))
	    (else #f)))
	 (lambda (key . args)
	   #f)))

(define-scwm-option *use-scwm-system-proc* #f
  "If #t, `execute' will use `scwm-system' instead of guile's `system'.
This works around a problem observed on pre-glibc Linux 2.0.34 i386
where SIGINT on the controlling tty (i.e., the one that started
scwm) propagates to the child processes.  The end result of the
possible bug is that xterms started by scwm are terminated if the
scwm that started them is terminated using a Ctrl-C to send it a SIGINT."
  #:type 'boolean
  #:group 'system)

(define-scwm-option *menu-font* (make-font "*helvetica*medium-r*12*")
  "The default menu font."
  #:type 'font
  #:group 'menu
  #:setter (lambda (font) (set! menu-font font))
  #:getter (lambda () menu-font))

(define-scwm-option *menu-title-font* (make-font "*helvetica*bold-r*12*")
  "The default menu title font."
  #:type 'font
  #:group 'menu
  #:setter (lambda (font) (set! menu-title-font font))
  #:getter (lambda () menu-title-font))


(define-scwm-option *menu-text-color* (make-color "black")
  "The default menu text color."
  #:type 'color
  #:group 'menu
  #:setter (lambda (color) (set! menu-text-color (maybe-make-color color)))
  #:getter (lambda () menu-text-color))
;; (scwm-option-favorites '*menu-text-color*)

(define-scwm-option *menu-bg-color* (make-color "grey75")
  "The default menu background color."
  #:type 'color
  #:group 'menu
  #:setter (lambda (color) (set! menu-bg-color (maybe-make-color color)))
  #:getter (lambda () menu-bg-color))

;; GJB:FIXME:: this option needs to be able to set the value to #f
(define-scwm-option *menu-hl-fg-color* #f
  "The default menu highlight fg/text color."
  #:type 'color
  #:group 'menu
  #:setter (lambda (color) (set! menu-hl-fg-color (maybe-make-color color)))
  #:getter (lambda () menu-hl-fg-color))

;; GJB:FIXME:: this option needs to be able to set the value to #f
(define-scwm-option *menu-hl-bg-color* #f
  "The default menu highlight background color."
  #:type 'color
  #:group 'menu
  #:setter (lambda (color) (set! menu-hl-bg-color (maybe-make-color color)))
  #:getter (lambda () menu-hl-bg-color))



(define-public display-width (car (display-size)))
(define-public display-height (cadr (display-size)))
(define-public display-depth (caddr (X-display-information)))

;; Convenience procedures for specifying positions and sizes.
(define-public (%x x)
  "Return the number of pixels that is X percent of the display width."
  (round/ (* x display-width) 100))

(define-public (%y y)
  "Return the number of pixels that is Y percent of the display height."
  (round/ (* y display-height) 100))

(define-public (pix->%x pix)
  "Return the percent of the display width that PIX is."
  (round/ (* pix 100) display-width))

(define-public (pix->%y pix)
  "Return the percent of the display height that PIX is."
  (round/ (* pix 100) display-height))

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

(define-public (vpx->vx x)
  "Convert from a viewport to a virtual X coordinate.
If X is #f, just return #f."
  (if x 
      (+ (car (viewport-position)) x)
      #f))

(define-public (vpy->vy y)
  "Convert from a viewport to a virtual Y coordinate.
If Y is #f, just return #f."
  (if y
      (+ (cadr (viewport-position)) y)
      #f))

(define-public (vx->vpx x)
  "Convert from a virtual to a viewport X coordinate.
If X is #f, just return #f."
  (if x
      (- x (car (viewport-position)))
      #f))

(define-public (vy->vpy y)
  "Convert from a virtual to a viewport Y coordinate.
If Y is #f, just return #f."
  (if y
      (- y (cadr (viewport-position)))
      #f))

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

(define-public (set-default-menu-foreground! fg)
  "Set the default color for menu text to FG."
  (set! menu-text-color (if (color? fg) fg (make-color fg))))
(define-public (set-default-menu-background! bg)
  "Set the default background for menus to BG."
  (set! menu-bg-color (if (color? bg) bg (make-color bg)))
  (if (not menu-side-bg-color-set) (set! menu-side-bg-color menu-bg-color)))
(define-public (set-default-menu-stipple! stipple)
  "Set the default color for stippled (inactive) menu text to STIPPLE."
  (set! menu-stipple-color (if (color? stipple) stipple (make-color stipple))))
(define-public (set-default-menu-font! font)
  "Set the default font for menu text to FONT."
  (set! menu-font (if (font? font) font (make-font font))))
(define-public (set-default-menu-title-font! font)
  "Set the default font for menu title text to FONT."
  (set! menu-title-font (if (font? font) font (make-font font))))
(define-public (set-default-menu-side-image! image)
  "Set the default menu side image to IMAGE."
  (set! menu-side-image (if (image? image) image (make-image image))))
(define-public (set-default-menu-side-background! bg)
  "Set the default background for the menu side image to BG.
If BG is #f, use the default menu background" 
  (cond (bg (set! menu-side-bg-color 
		  (if (color? bg) bg (make-color bg)))
	    (set! menu-side-bg-color-set #t))
	(else (set! menu-side-bg-color menu-bg-color)
	      (set! menu-side-bg-color-set #f))))
(define-public (set-default-menu-bg-image! image)
  "Set the default menu background image to IMAGE."
  (set! menu-bg-image (if (image? image) image (make-image image))))
(define-public (set-default-menu-look! look)
  "Set the default menu look to LOOK."
  (if (menu-look? look) (set! menu-look look) (error "bad look")))
(define-public (set-default-menu-hl-fg-color! fg)
  "Set the default menu highlight foreground color to FG."
  (set! menu-hl-fg-color (if (color? fg) fg (make-color fg))))
(define-public (set-default-menu-hl-bg-color! bg)
  "Set the default menu highlight background color to BG."
  (set! menu-hl-bg-color (if (color? bg) bg (make-color bg))))

;;(define*-public (set-window-foreground! fg #&optional (w (get-window)))
;;  (set-window-colors! fg #f w))

;;(define*-public (set-window-background! bg #&optional (w (get-window)))
;;  (set-window-colors! #f bg w))

(define*-public (set-highlight-colors! #&optional (bg #f) (fg #f))
  "Set the highlight window's background color to BG, foreground color to FG.
The \"highlight window\" is the window with the current input focus."
  (if bg (set-highlight-background! bg win))
  (if fg (set-highlight-foreground! fg win)))

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
  (let ((pos (viewport-position)))
    (if (not (sticky-window? win))
	(begin
	  (if x (set! x (+ x (car pos))))
	  (if y (set! y (+ y (cadr pos)))))))
  (move-window x y win))

;; Give move-to a better name, too
;; GJB:FIXME:DOC: Can this have a doc string?
(define-public move-window-viewport-position move-to)

(define-public (delta-position xy-list dx dy)
  "Return a new coordinate list that is DX,DY offset from XY-LIST.
E.g., if XY-LIST is (2 10) and DX is 5, DY is 7, returns (7 17)."
  (map + xy-list (list dx dy)))
;; (delta-position '(2 10) 5 7)

(define*-public (move-window-relative x y #&optional (win (get-window)))
  "Move WIN X, Y pixels from where it is currently.
Positive X moves right, negative moves left.
Positive Y moves down, negative moves up."
  (with-window win
	       (let ((pos (window-viewport-position)))
		 (apply move-to (delta-position pos x y)))))

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
    (if (sticky-window? win) pos
	(apply virtual->viewport pos))))

(define*-public (window-virtual-position #&optional (win (get-window)))
  "Return the virtual position of WIN in pixels.
If WIN is sticky, this returns the position of the window in the
current viewport."
  (let ((pos (window-position win)))
    (if (sticky-window? win) (apply viewport->virtual pos)
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

(define-public (use-placement-hint win)
  "Do nothing when placing WIN, just let it be where hinted to be.
This is useful for, e.g., external programs such as the GNOME desktop
gmc which expect their windows to go where asked (like the desktop
icons in gmc)."
  #t)

;; FIXJTL: image-align - to work like others, C code has to dereference
;; symbols that aren't one of 'top, 'bottom or 'center
(define*-public (menu-style #&key
		     (fg #f) (foreground #f)
		     (bg #f) (background #f)
		     (hl-fg #f) (hl-foreground #f)
		     (hl-bg #f) (hl-background #f)
		     (bg-image #f)
		     (stipple #f) (font #f)
		     (title-font #f)
		     (look #f)
		     (side-image #f) (side-bg 'unset))
  "Set various properites for the menus.
See `make-menu' for options on creation of individual menus."
  (if (or fg foreground) (set-default-menu-foreground! (or fg foreground)))
  (if (or bg background) (set-default-menu-background! (or bg background)))
  (if (or hl-fg hl-foreground) (set-default-menu-hl-fg-color! (or hl-fg hl-foreground)))
  (if (or hl-bg hl-background) (set-default-menu-hl-bg-color! (or hl-bg hl-background)))
  (if stipple (set-default-menu-stipple! stipple))
  (if font (set-default-menu-font! font))
  (if title-font (set-default-menu-title-font! title-font))
  (if look (set-default-menu-look! look))
  (if side-image (set-default-menu-side-image! side-image))
  (if (not (eq? side-bg 'unset)) (set-default-menu-side-background! side-bg))
  (if bg-image (set-default-menu-bg-image! bg-image)))

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

(define-public (menu-title label . rest)
  "Return a menuitem object that is a title.
All arguments that `menuitem' takes are accepted as usual,
except the font defaults to `*menu-title-font*' instead of
`*menu-font*'."
  (apply menuitem (append (list label #:font 'menu-title-font) rest)))

(define*-public (menuitem label #&key image-above image-left
			  (fg #f) (bg #f) (font #f)
			  extra-label action submenu hover-action unhover-action
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
  (if (string? bg) (set! bg (make-color bg)))
  (if (string? fg) (set! fg (make-color fg)))
  (if (string? font) (set! font (make-font font)))
  (if (string? action)			;; permit "xterm" to mean (execute "xterm")
      (let ((program-name action))
	(set! action (lambda () (execute program-name)))))
  (if (and (bound? action) (bound? submenu))
      (error "Cannot give both an action and a submenu"))
  (if (bound? submenu)
      (set! action submenu))
  (let ((mi
	 (make-menuitem label action extra-label image-above image-left
			hover-action unhover-action hotkey-prefs (bound? submenu))))
    (if (or fg bg) (set-menuitem-colors! mi fg bg))
    (if font (set-menuitem-font! mi font))
    mi))


(define*-public (menu list-of-menuitems #&key
		      (image-side 'menu-side-image)
		      (image-align 'top)
		      (color-bg-image-side 'menu-side-bg-color)
		      (image-bg 'menu-bg-image)
		      (color-text 'menu-text-color)
		      (color-bg 'menu-bg-color)
		      (color-stipple 'menu-stipple-color)
		      (hl-color-fg 'menu-hl-fg-color)
		      (hl-color-bg 'menu-hl-bg-color)
		      (hl-relief? #t)
		      (font 'menu-font)
		      (look 'menu-look)
		      popup-delay hover-delay
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
    (and (bound? popup-delay) popup-delay (set-menu-popup-delay! menu popup-delay))
    (and (bound? hover-delay) hover-delay (set-menu-hover-delay! menu hover-delay))
    (if (or hl-color-bg hl-color-fg) 
	(set-menu-highlight-colors! menu hl-color-fg hl-color-bg))
    (set-menu-highlight-relief! menu hl-relief?)
    (set-menu-look! menu look)
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
  (lambda* () "Created by `exe' higher-order function" 
	   (interactive) (execute command)))

(define-scwm-option *xterm-command* (or (getenv "XTERM_PROGRAM_NAME") "xterm")
  "The command to run when a new xterm window is requested.
The string given should refer to a binary or script in the
path and should take a \"-e\" argument of what to run."
  #:type 'command
  #:group 'system
  #:favorites '("xterm" "color_xterm" "nxterm"))

(define-scwm-option *remote-shell-command* "telnet"
  "The command to use to start a remote shell.
It should take a first (non-option) argument of
the hostname to connect to.  \"ssh\" \"rsh\" and \"telnet\"
are each reasonable choices."
  #:type 'command
  #:group 'system
  #:favorites '("telnet" "rsh" "ssh"))


(define-scwm-option *xterm-user-shell-options* ""
  "Any extra options to give to xterm for a standard user xterm process.
For example, to start a different interactive shell, you would use
\"-e zsh\"."
  #:type 'command
  #:group 'system
  #:favorites '("-e zsh" "-e tcsh" "-e start-zsh"))

(define-public (run-in-xterm cmd . opts)
  "Return a procedure that runs CMD in an xterm.
Uses the variable `*xterm-command*' to determine how
to run an xterm.  CMD may include options to the command.
The rest of the arguments are passed as options to the xterm command."
  (exe (string-append (optget *xterm-command*)
                      (apply string-append
                             (map (lambda (st) (string-append " " st)) opts))
                      " -e " cmd)))

(define*-public (start-xterm #&optional (opt (optget *xterm-user-shell-options*)))
  "Start an xterm using `*xterm-command*' and `*xterm-user-shell-options*'."
  (interactive)
  (execute (string-append (optget *xterm-command*) " " opt)))

(define*-public (xterm-other-host hostname)
  "Run an xterm on machine HOSTNAME.
Starts the XTerm with resource \"remotexterm\" and uses
`*xterm-command*' to determine which terminal program
to use and `*remote-shell-command*' to determine how to
start the shell remotely."
  (exe (string-append (optget *xterm-command*)
		      " -name remotexterm -T " hostname " -n " hostname 
		      " -e sh -c '" *remote-shell-command* " " hostname "'")))

(defmacro-public thunk (proc)
  `(lambda args (apply ,proc args)))

;; Only define if not already defined by Guile
(if (not (defined? 'remove-hook!))
    (defmacro-public remove-hook! (hook proc)
      `(if (memq ,proc ,hook)
           (set! ,hook
                 (delq! ,proc ,hook)))))


(defmacro-public with-window (win . body)
;;;** Bind the window-context to WIN while evaluating BODY.
;;; All `get-window' calls within BODY will return WIN.
  `(let ((old-window-context (window-context)))
     (dynamic-wind
      (lambda () (set-window-context! ,win))
      (lambda () ,@body)
      (lambda () (set-window-context! old-window-context)))))

(defmacro-public with-message-window-shown (mwn . body)
;;;** Display message window MWN while evaluating BODY.
;;; If BODY throws, the MWN is guaranteed to be removed from the display.
  `(let ((msgwin ,mwn))
     (dynamic-wind
      (lambda () (and (message-window? msgwin) (message-window-show! msgwin)))
      (lambda () ,@body)
      (lambda () (and (message-window? msgwin) (message-window-hide! msgwin))))))
  
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

(define-public (scwm-is-constraint-enabled?)
  "Return #t if scwm has the constraint solver primitives, #f otherwise."
  (bound? scwm-set-master-solver!))

(define-public (scwm-system cmd)
  "Run CMD using /bin/sh -c CMD and return a list: (exit-status child-pid).
Note that the child pid is of the executed sh, not CMD.
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
	(list (cdr (waitpid child-pid)) child-pid)))
     (else ;; child
      (begin
	(setpgid 0 0)
	(execlp "/bin/sh" "sh" "-c" cmd))
      ))))

(define-public (execute command)
  "Execute COMMAND in the background.
See also `execute-with-pidprop' if you want to know the
process id of COMMAND and want to use `window-id' to be
able to map back from the windows the process creates
to the process id."
  ((if *use-scwm-system-proc* scwm-system system) (string-append "exec " command " &")))

;; (execute-with-pidprop "xeyes")
;; (use-scwm-modules xprop-extras)
;; (window-pid (get-window))

;; GJB:FIXME:: switch to this after testing it
;; From Jim Blandy -- his [better] version of 
;; my scwm-system, above --09/26/98 gjb
(define-public (background-system command)
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


(define*-public (select-window #&optional (cursor #f) (release? #f))
  "Select a window interactively, and return the specified window.
Use a special cursor and let the user click to select the window. 
The optional CURSOR argument can be either a cursor object or #t to
indicate to use the \"skull and cross-bones\" kill cursor (recommended for destructive
operations like delete-window and destroy-window).
The optional argument RELEASE? indicates whether to wait
for a mouse release or act immediately on the click. 
Returns #f if no window was selected."
  (let ((win (car (select-viewport-position cursor release?))))
    (if (window? win) win #f)))

(define*-public (caught-error descriptor args)
  "Display an error message for a caught error.
DESCRIPTOR is a description to be printed, ARGS is the
argument to the catch exception lambda.
<example>
(catch #t
       (lambda () (+ 'f 2))
       (lambda args (caught-error \"Caught error\\n:\" args)))
</example>"
  (display descriptor (current-error-port))
  (apply display-error (append (list #f (current-error-port)) (cdr args))))

;;; stack port subr message args rest
;;; (display-error #f (current-output-port) 'foo "foo: %S" (list 1) #f)

(define*-public (select-window-interactively #&optional (msg #f) (message-window #f))
  "Return an interactively-selected window after prompting (optionally) with MSG.
If given, use message window MESSAGE-WINDOW to display the message, otherwise create
a new message window."
  (if msg
      (let ((msgwin (or (and (message-window? message-window)
			     (begin 
			       (message-window-set-message! message-window msg)
			       message-window))
			(make-message-window msg))))
	(with-message-window-shown msgwin (select-window)))
      (select-window)))
;; (select-window-interactively "foo")
;; (select-viewport-position)


(define-public (run-dot-xclients-script)
  "Runs the ~/.xclients script."
  (system "$HOME/.xclients &"))

(define-public (run-dot-xclients-at-startup)
  "After done reading your startup file, run your ~/.xclients script.
Uses the `startup-hook' and `run-dot-xclients-script' to do so."
  (add-hook! startup-hook (lambda () (if (not (restarted?)) (run-dot-xclients-script)))))

(define-public (keycode->keysym keycode)
  "Return a string containing the X11 keysym for key with code KEYCODE."
  (keymask-keycode->string 0 keycode))

(define*-public (window-class #&optional (win (get-window)))
  "Return the window resources class of WIN. 
WIN defaults to the window context in the usual way if not specified. 
Returns \"NoClass\" if the window has not set its class.
See also `window-class-hint'."
  (let ((prop (X-property-get win "WM_CLASS")))
    (if prop (cadar prop) "NoClass")))

(define*-public (window-resource #&optional (win (get-window)))
  "Return the window resource instance of WIN. 
WIN defaults to the window context in the usual way if not specified. 
Returns \"NoResource\" if the window has not set its resource name.
See also `window-resource-hint'."
  (let ((prop (X-property-get win "WM_CLASS")))
    (if prop (caar prop) "NoResource")))

(define-public (window-visible-frame-size win)
  "Return the visible frame size of WIN.
This is just the `window-frame-size' unless WIN is shaded in
which case the height is just the titlebar height."
  (let ((size (window-frame-size win)))
    (if (shaded-window? win)
	(list (car size) (cadr (window-decoration-size win)))
	(window-frame-size win))))

(define-public (make-image-or-warn filename)
  "Return an image object for FILENAME and report a warning if it fails.
See `make-image' for details.  Return value is #f on failure,
but no error is thrown; `image-not-found-message' is used to 
write a warning instead."
  (let ((answer (make-image filename)))
    (or answer
	(begin
	  (image-not-found-message filename)
	  #f))))

(define-public (image-not-found-message filename)
  "Report a missing image filename, but do not error."
  (display (string-append "Could not find image `" filename 
			  "' --- perhaps your image-load-path is wrong "
			  "or you need to install scwm/pixmaps or the "
			  "scwm-icons package?\n")))
