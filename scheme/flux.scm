;;; $Id$
;;; flux.scm
;;; (C) 1998 Sam Steingold, Greg J. Badros, and Maciej Stachowiak
;;;
;;; This are functions used by various sample .scwmrc, but not necessarily
;;; stabilized even as well as the other files in scheme/*.scm
;;; Expect the semantics of these functions to change, and don't
;;; be surprised if some even completely disappear (as we figure out a better
;;; way to do things)



(define-module (app scwm flux)
  :use-module (ice-9 regex)
  :use-module (app scwm base)
  :use-module (app scwm wininfo)
  :use-module (app scwm winlist)
  :use-module (app scwm winops)
  :use-module (app scwm string-prompt)
  :use-module (app scwm path-cache)
  :use-module (app scwm optargs))



(define-public user-init-file (string-append (user-home) "/.scwmrc"))

;; The #t arguments should perhaps instead be a closure
;; returning whether an opaque move/resize is desired

(define-public (interactive-move-window-with-focus)
  "Interactively move the window which currently has the focus.
`move-opaquely?' is used to control whether a rubberband
outline or the window itself is moved."
  (let ((w (current-window-with-focus))) (and w (interactive-move w))))

(define-public (interactive-resize-window-with-focus)
  "Interactively resize the window which currently has the focus.
`resize-opaquely?' is used to control whether a rubberband
outline or the window itself is resized."
  (let ((w (current-window-with-focus))) (and w (interactive-resize w))))

(define-public (interactive-move-window-with-pointer)
  "Interactively move the window which currently contains the pointer.
`move-opaquely?' is used to control whether a rubberband
outline or the window itself is moved."
  (let ((w (current-window-with-pointer))) (and w (interactive-move w))))

(define-public (interactive-resize-window-with-pointer)
  "Interactively resize the window which currently contains the pointer.
`resize-opaquely?' is used to control whether a rubberband
outline or the window itself is resized."
  (let ((w (current-window-with-pointer))) (and w (interactive-resize w))))

(define-public (toggle-max-vert)
  "Toggle the current window's maximized-vertically state."
  (toggle-maximize 0 (%y 100)))

(define-public (toggle-max-horz)
  "Toggle the current window's maximized-horizontally state."
  (toggle-maximize (%x 100) 0))

(define-public (toggle-max-both)
  "Toggle the current window's maximization (both vertically and horizontally)."
  (toggle-maximize (%x 100) (%y 100)))

(define-public (toggle-max-vert-part)
  "Toggle the current window's maximization-vertically to 95% of the screen height."
  (toggle-maximize 0 (%y 95)))

(define-public (wiggle-window)
  "Animatedly window shade and then unshade the current window.
Just a toy--- perhaps could be useful to call attention to a window."
  (let ((w (get-window))) (window-shade w #t) (un-window-shade w #t)))

(define-public (write-all port . lst)
  "Write all arguments into the port. #t means `current-output-port'."
  (if (eq? port #t) (set! port (current-output-port)))
  (do ((zz lst (cdr zz))) ((null? zz))
    (if (string? (car zz)) (display (car zz) port) (write (car zz) port))))

(define-public (to-string . rest)
  "Dump all arguments into a string."
  (with-output-to-string (lambda () (apply write-all #t rest))))

(define-public (system-info-string)
  "Return a string with various system information.
Use `show-system-info' to display it in a window."
  (let ((vv (X-version-information)) (dd (X-display-information)))
    (apply
     to-string "Guile verion:\t\t" (version)
     "\nLibguile timestamp:\t" (libguile-config-stamp)
     "\nSCWM version:\t\t" (scwm-version)
     "\nFrom repository date:\t" (scwm-version-date)
     "\nRestarted:\t\t" (bool->str (restarted?))
     "\nDisplay Size:\t\t" (size->str (display-size))
     "\nDesk Size:\t\t" (size->str (desk-size))
     "\nViewport Position:\t" (size->str (viewport-position))
     "\nPointer:\t\t" (size->str (pointer-position))
     "\nCurrent Desk:\t\t" (number->string (current-desk))
     "\nX vendor:\t\t" (caddr vv) "; version: " (number->string (car vv)) "."
     (number->string (cadr vv)) "; release: " (number->string (cadddr vv))
     "\nX Display:\n\tResolution:\t" (size->str dd) "\n\tColor:\t\t"
     (list-ref dd 4) " (depth: " (number->string (caddr dd))
     "; bits per RGB: " (number->string (cadddr dd)) ")\nimage-load-path:\n"
     (map (lambda (st) (string-append "\t" st "\n")) image-load-path))))

;; CRW:FIXME:: This should be merged with make-context-menu in
;; std-menus.scm

;; I (CRW) changed "vi" to "emacs" below.  If anybody feels strongly
;; that the default should be "vi", at least make it "xterm -e vi"
;; instead of just "vi".
(define-public (make-file-menu file . rest)
  "Return a menu-object for viewing or editing FILE.
REST is a list of other menu-items to include in the returned menu."
  (menu (append! (list (menuitem "View" #:action (show-file file))
		       (menuitem "Edit" #:action
				 (string-append (or (getenv "EDITOR") "emacs")
						" " file)))
		 rest)))

(define-public (quotify-single-quotes str)
  "Return a string that has single quote characters backslashified."
  (regexp-substitute/global #f "'" str 'pre "'\"'\"'" 'post))

;;; FIXGJB: how set width of an xmessage?
(define-public (message . str)
  "Display the string arguments STR in a message window.
Requires the program `xmessage'."
  (execute (string-append "echo -e \'"
			  (quotify-single-quotes (apply string-append str))
			   "\'| xmessage -file - -default okay -nearmouse")))

(define-public (show-mesg . str)
  "Return a lambda to display the string arguments STR in a message window.
See also `message'."
  (lambda () (apply message str)))

(define-public (show-file filename)
  "Return a lambda to display the contents of filename in a window."
  (exe (string-append "xmessage -default okay -nearmouse -file " filename)))

(define-public (show-com com)
  "Return a lambda to show the stdout generated by the COM shell pipeline."
  (exe (string-append com "| xmessage -file - -default okay -nearmouse")))

(define-public (bool->str arg)
  "Return the string \"false\" if ARG is #f, \"true\" otherwise."
  (if arg "true" "false"))

(define*-public (size->str sz #&optional (sep "x"))
  "Convert a two-element list to a string.
Use the optional second argument as the separator."
  (string-append (number->string (car sz)) sep (number->string (cadr sz))))

(define*-public (window-info #&optional (win (get-window)))
  "Display information about WIN in a message window."
  (message
   "Window ID:\t\t" (number->string (window-id win))
   "\nWindow Frame ID:\t" (number->string (window-frame-id win))
   "\nTitle:\t\t\t\"" (window-title win) "\""
   "\nVirtual Position:\t\t" (size->str (window-position win))
   "\nViewport Position:\t\t" (size->str (window-viewport-position win))
   "\nSize:\t\t\t" (size->str (window-frame-size win))
   "\nDesk:\t\t\t" (number->string (window-desk win)) "\nClass:\t\t\t\""
   (window-class win) "\"\nResource:\t\t\"" (window-resource win)
   "\"\nBorder Normal:\t\t" (bool->str (border-normal? win))
   "\nDeletable:\t\t" (bool->str (window-deletable? win))
   "\nIconified:\t\t" (bool->str (iconified? win))
   "\nKept On Top:\t\t" (bool->str (kept-on-top? win))
   "\nTransient:\t\t" (bool->str (transient? win))
   "\nRaised:\t\t\t" (bool->str (raised? win))
   "\nShaded:\t\t\t" (bool->str (window-shaded? win))
   "\nShaped:\t\t\t" (bool->str (window-shaped? win))
   "\nIcon Shaped:\t\t" (bool->str (window-icon-shaped? win))
   "\nSticky Icon:\t\t" (bool->str (icon-sticky? win))
   "\nSticky:\t\t\t" (bool->str (sticky? win))
   "\nTitle Bar Shown:\t" (bool->str (titlebar-shown? win))))

(define-public (show-system-info)
  "Display the `system-info-string' system details in a window."
  (message (system-info-string)))

;; FIXGJB: stop at tabs, too
(define (first-word s)
  "Return the first word of S (up to but not including first space char."
  (let ((i (string-index s #\space)))
    (if i (substring s 0 i) s)))
;;(first-word "foo bar") => "foo"

(define-public (make-menuitems-from-menu-information-list menu-info-list)
  "Return a list of menu-items from a list of detailed programs list.
The format is subject to change.  See sample.scwmrc/gjb.scwmrc for
example usage."
  (cons menu-title
	(cons menu-separator
	      (map (lambda (elem)
		     (let ((title (car elem))
			   (mini-icon (cadr elem))
			   (icon (caddr elem))
			   (exename (cadddr elem)))
		       (if (cached-program-exists? (first-word exename))
			   (menuitem
                            title #:action exename #:image-left
                            (if mini-icon
                                (string-append "mini-" mini-icon ".xpm") #f)
                            ;; #:icon (if icon (string-append icon ".xpm") #f)
			    )
			   #f)))
		   menu-info-list))))

(define-public (key-mouse-moves modifiers pct-of-screen left down up right)
  "Bind four keys to move the mouse in compass directions by PCT-OF-SCREEN.
MODIFIERS specifies which modifiers must be depressed for the bindings
to be active.
LEFT, DOWN, UP, and RIGHT are the four keysym names to use for each
of the directions."
  (bind-key 'all (string-append modifiers "-" left)
	    (lambda () (move-pointer (%x (- pct-of-screen)) 0)))
  (bind-key 'all (string-append modifiers "-" down)
	    (lambda () (move-pointer 0 (%y pct-of-screen))))
  (bind-key 'all (string-append modifiers "-" up)
	    (lambda () (move-pointer 0 (%y (- pct-of-screen)))))
  (bind-key 'all (string-append modifiers "-" right)
	    (lambda () (move-pointer (%x pct-of-screen) 0))))

(define-public (key-viewport-moves modifiers pct-of-screen left down up right)
  "Bind four keys to move the viewport in compass directions by PCT-OF-SCREEN.
MODIFIERS specifies which modifiers must be depressed for the bindings
to be active.
LEFT, DOWN, UP, and RIGHT are the four keysym names to use for each
of the directions."
  (bind-key 'all (string-append modifiers "-" left)
	    (lambda () (move-viewport (%x (- pct-of-screen)) 0)))
  (bind-key 'all (string-append modifiers "-" down)
	    (lambda () (move-viewport 0 (%y pct-of-screen))))
  (bind-key 'all (string-append modifiers "-" up)
	    (lambda () (move-viewport 0 (%y (- pct-of-screen)))))
  (bind-key 'all (string-append modifiers "-" right)
	    (lambda () (move-viewport (%x pct-of-screen) 0))))

(define-public (sleep-ms ms)
  "Delay for MS milliseconds.
Note that timer-hooks are much more useful in nearly all
cases.  See `add-timer-hook!'."
  (select '() '() '() 0 (* 1000 ms)))

(define-public (printable-char->keysym-string char)
  "Return the keysym string corresponding to a printable character.
CHAR is a scheme character.  The return value is appropriate for
use by `send-key-press'.  See also `X-synthetic-send-string'."
  (let ((charval (char->integer char))
	(char-keysym-alist
	 '((#\space . "space")
	   (#\newline . "Linefeed")
	   (#\cr . "Return")
	   (#\esc . "Escape")
	   (#\bs . "BackSpace")
	   (#\del . "Delete")
	   (#\tab . "Tab")
	   (#\! . "S-1") ;; "exclam"
	   (#\" . "S-apostrophe") ;; "quotedbl"
	   (#\# . "S-3") ;; "numbersign"
	   (#\$ . "S-4") ;; "dollar"
	   (#\% . "S-5") ;; "percent"
	   (#\& . "S-7") ;; "ampersand"
	   (#\' . "apostrophe")
	   (#\( . "S-9") ;; "parenleft"
	   (#\) . "S-0") ;; "parenright"
	   (#\* . "S-8") ;; "asterisk"
	   (#\+ . "S-equal") ;; "plus"
	   (#\, . "comma")
	   (#\- . "minus")
	   (#\. . "period")
	   (#\/ . "slash")
	   (#\: . "S-semicolon") ;; "colon"
	   (#\; . "semicolon")
	   (#\< . "less")
	   (#\= . "equal")
	   (#\> . "S-period") ;; "greater"
	   (#\? . "S-slash") ;; "question"
	   (#\@ . "S-2") ;; "at"
	   (#\[ . "bracketleft")
	   (#\\ . "backslash")
	   (#\] . "bracketright")
	   (#\^ . "S-6") ;; "caret"
	   (#\_ . "S-minus") ;; "underscore"
	   (#\` . "grave")
	   (#\' . "quoteleft")
	   (#\{ . "S-bracketleft") ;; "braceleft"
	   (#\| . "S-backslash") ;; "bar"
	   (#\} . "S-bracketright") ;; "brackeright"
	   (#\~ . "S-grave") ;; "asciitilde"
	   )))
    (let ((cell (assq char char-keysym-alist)))
      (cond
       (cell
	(cdr cell))
       ((< charval 32) (string-append "C-"
				      (make-string 1 (integer->char
						      (+ 64 charval)))))
       (#t (make-string 1 char))))))
;; (printable-char->keysym-string "")
;; (X-synthetic-send-string "!@#$%^&*()_+[]\\{}|;':\",./<>?`~" (get-window))


(define*-public (X-synthetic-send-string str #&optional (win (get-window)))
  "Send string STR to WIN via synthetic X events.
Note that some programs (e.g., xterm) by default do not
honour synthetic key events as they are a security hole."
  (let ((i 0))
    (while (< i (string-length str))
	   (send-key-press
	    (printable-char->keysym-string (string-ref str i)) win)
	   (set! i (+ 1 i)))))

;; from Harvey Stein; rewritten by Carl Witty
(define-public (find-window-by pred)
  "Return a window satisfying predicate PRED.
If there are multiple such windows, an unspecified one of them
will be returned."
  (let ((wlist (list-windows #:only pred)))
    (if (not (null? wlist))
	(car wlist)
	#f)))

(define-public (find-window-by-name window-name)
  "Return a window with name WINDOW-NAME.
If there are multiple such windows, an unspecified one of them
will be returned."
  (find-window-by (lambda (w)
		    (string=? (window-title w) window-name))))

(define-public (find-window-by-class-resource class resource)
  "Return a window by its CLASS and RESOURCE names (as strings).
If there are multiple such windows, an unspecified one of them
will be returned."
  (find-window-by (lambda (w)
		    (and (string=? (window-class w) class)
			 (string=? (window-resource w) resource)))))


(define (list-without-elem l e)
  (cond ((null? l) l)
	((eq? (car l) e) (cdr l))
	(else (cons (car l) (list-without-elem (cdr l) e)))))

(define-public (select-window-group)
  (do ((w #f)
       (wlist '())
       (cwin-selected 0)
       (w #f)
       (done #f))
      (done
       wlist)
    (set! w (select-window-interactively
	     (string-append "select #" (number->string cwin-selected))))
    (if w
	(if (memq w wlist)
	    (begin
	      ;; remove w from wlist
	      (set! wlist (list-without-elem wlist w))
	      (unflash-window w)
	      (set! cwin-selected (- cwin-selected 1)))
	    (begin
	      (set! wlist (cons w wlist))
	      (flash-window w #:unflash-delay #f)
	      (set! cwin-selected (+ cwin-selected 1))))
	(set! done #t))))
;; (define wg (select-window-group))
;; (for-each (lambda (w) (unflash-window w)) wg)
;; (unflash-window)

(define-public (select-window-interactively-and-highlight)
  (let ((w (select-window-interactively)))
    (flash-window w #:unflash-delay #f)
    w))
;; (unflash-window (select-window-interactively))


;; Returns them in reverse the order they were selected
;; should probably turn off the invalid interaction hook
;; or provide a way of telling select-window-interactively that
;; the root window is not an error
(define*-public (select-multiple-windows-interactively
		 #&optional (max 32000) (proc-when-selected #f))
  "Return a list of user-selected windows, up to MAX.
The list is in the reverse order from the way by which they were selected.
PROC-WHEN-SELECTED will be run on each window as it is selected."
  (if (not (integer? max))
      (set! max 32000))
  (do ((w '())
       (wlist '() (cons w wlist))
       (i 0 (+ 1 i)))
      ((or (not w) (>= i max))
       (if w wlist
	   (cdr wlist)))
    (set! w (select-window-interactively (string-append "select #" (number->string i))))
    (if (and proc-when-selected w)
	(proc-when-selected w))))

;; e.g.
;;(select-multiple-windows-interactively 10)
;;(restack-windows (select-multiple-windows-interactively 3))

(define*-public (select-window-from-window-list #&key
						(only '()) (except '())
						(ignore-winlist-skip #f))
  "Permit selecting a window from a window list.
Return the selected window object, or #f if none was selected"
  (show-window-list-menu #:only only #:except except
			 #:flash-window-proc
			 (lambda (w) (flash-window w #:unflash-delay #f))
			 #:unflash-window-proc
			 (lambda (w) (unflash-window w))
			 #:ignore-winlist-skip ignore-winlist-skip #:proc (lambda (w) w)))

;; e.g.
;; (let ((w (select-window-from-window-list #:only iconified?)))
;;  (deiconify w) (move-to 0 0 w))
;; (select-window-from-window-list)
;; (unflash-window (get-window))

(define-public (color->string color)
  "Convert scwm color object COLOR into an X11 name of that color.
The resulting string can, e.g., be used in command lines for executing
other applications."
  (color-property color 'name))

(define-public (set-window-title! win title)
  "Change the window title X-Property of WIN to TITLE.
WIN is a Scwm window object, TITLE is a string.  This procedure alters the
window title by changing the WM_NAME X-Property."
  (X-property-set! win "WM_NAME" title))

(define-public (get-wm-command win)
  "Get the \"WM_COMMAND\" X-Property of WIN and return that string.
WIN is a Scwm window object. The \"WM_COMMAND\" X-Property is the application's
notion of what the command line was used to run the application."
  (let ((prop (X-property-get win "WM_COMMAND")))
    (and (list? prop) (car prop))))

(define-public (sec->usec sec)
  "Convert SEC seconds into an equivalent number of microseconds.
Especially useful for add-hook! and other timing related procedures
that take microseconds."
  (* 1000000 sec))

(define-public (ms->usec ms)
  "Convert MS milliseconds into an equivalent number of microseconds.
Especially useful for add-hook! and other timing related procedures
that take microseconds."
  (* 1000 sec))


;; We need accessors for window background information,
;; and window-hilight background information
(define*-public (flash-window #&optional (win (get-window)) #&key
			      (color (make-color "red"))
			      (unflash-delay .5))
  "Flash WIN's titlebar and boundary color to COLOR for UNFLASH-DELAY seconds.
UNFLASH-DELAY may be #f to not automatically revert back to the original
color.  See `unflash-window'."
  (set-object-property! win 'old-bg (cadr (get-window-colors win)))
  (set-object-property! win 'old-hi-bg (cadr (get-window-highlight-colors win)))
  (set-window-background! color win)
  (set-window-highlight-background! color win)
  (if (number? unflash-delay)
      (add-timer-hook! (sec->usec unflash-delay)
		       (lambda ()
			 (unflash-window win)))))


(define*-public (unflash-window #&optional (win (get-window)))
  "Revert WIN's titlebar and boundary color to state before a `flash-window'."
  (let ((old-bg (object-property win 'old-bg))
	(old-hi-bg (object-property win 'old-hi-bg)))
    (set-window-background! old-bg win)
    (set-window-highlight-background! old-hi-bg win)))


(define-public (make-string-usable-for-resource-key string)
  "Return a converted string from STRING that can be used as an X resource key.
The returned string will have all non-alphanumeric characters replaced with
underscores, so that the resulting string can be used as a key for
`X-resource-get' and `X-resource-put'."
  (regexp-substitute/global
   #f "[^a-zA-Z_0-9]" string
   'pre (lambda (match) "_")
   'post))

;;; (make-string-usable-for-resource-key "foo bar baz")
;;; (make-string-usable-for-resource-key "foo*bar.baz")


;; From S.Senda -- Aug 3, 1998
;;;;;;;; rlogin menu making from .rhosts file ;;;;;;;;;

(define-public (make-rhosts-menu)
  "Returns a menu which lets you rlogin to each host mentioned in your .rhosts"
  (false-if-exception
   (let* ((rhostfn (string-append (user-home) "/.rhosts"))
	  (termprog "xterm")
	  (p (open-input-file rhostfn))
	  (ret '())
	  (ap (lambda (a)
		(set! ret (append ret (list a)))))
	  (mm (lambda (h u)
		(menuitem h #:action
			  (lambda () (execute
				      (string-append termprog " -e rlogin "
						     h " -l " u))))))
      )
    (ap (menuitem ".rhosts" #f))
    (ap menu-title)
    (do ((l (read-line p 'trim) (read-line p 'trim)))
	((eof-object? l) ret)
      (cond ((string-match "([^ \t]+)[ \t]+([^ \t]+)" l)
	     => (lambda (m)
		  (ap (mm (match:substring m 1)   ; machine name
			  (match:substring m 2))) ; user name
		  ))))
    (ap menu-title)
    (ap (menuitem "reread .rhosts file" #:action
	    (lambda () (set! rhosts-menu (make-rhosts-menu)))))
    (menu ret)
)))

;; sds: users should call this function themselves
;;(define-public rhosts-menu (make-rhosts-menu))

(define-public (X-cut-buffer-string)
  "Return the text of the CUT_BUFFER0 property of the root window.
This is the cut text selected by X clients.  Returns #f if the
CUT_BUFFER0 property is not a string."
  (let ((l (X-property-get 'root-window "CUT_BUFFER0")))
    (if (string=? (cadr l) "STRING")
	(car l)
	#f)))

(define*-public (display-message-briefly msg #&optional (sec-timeout 3))
  "Display MSG in the message window for SEC-TIMEOUT seconds.
See `display-message' for details about MSG."
  (let ((mwn (make-message-window msg)))
    (message-window-show! mwn)
    (add-timer-hook! (sec->usec sec-timeout)
		     (lambda () (message-window-hide! mwn)))))

(define-public (close-all-xlogo-windows)
  "Close each window with class == XLogo.
Greg uses XLogo windows as a sample window, so this
is useful for clearing the xlogos away when there get to
be more than desired."
  (for-each (lambda (w) (close-window w))
	    (list-windows #:only
			  (lambda (w)
			    (string=? (window-class w) "XLogo")))))

(define-public (delta-position xy-list dx dy)
  "Return a new coordinate list that is DX,DY offset from XY-LIST.
E.g., if XY-LIST is (2 10) and DX is 5, DY is 7, returns (7 17)."
  (map + xy-list (list dx dy)))
;; (delta-position '(2 10) 5 7)

(define*-public (move-window-relative dx dy #&optional (win (get-window)))
  "Move WIN from its current position by (dx,dy) pixels."
  (with-window
   win
   (let ((pos (window-viewport-position)))
     (apply move-to (delta-position pos dx dy)))))
;; (move-window-relative 10 10)

;; useful for debugging/testing
;;(set-X-server-synchronize! #t)

;; Get a visible netscape window, or any netscape window
;; if none are visible
(define-public (netscape-win)
  "Return a netscape window, prefer a visible netscape window.
May error if no netscape windows are present."
  (let* ((ns-wins
	 (list-windows #:only
		       (lambda (w)
			 (and (string=? (window-class w) "Netscape")
			      (string=? (window-resource w) "Navigator")))))
	 (win (car ns-wins)))
    (for-each (lambda (w) (if (visible? w) (set! win w))) ns-wins)
    win))


(define*-public (X-atomic-property-set-if-unset! window name value #&optional
						(type "STRING") (format 8))
  "Set property NAME on WINDOW to VALUE, if it's currently unset.
Returns #f if the property is already set, #t otherwise.
TYPE and FORMAT are as in X-property-set!"
  (with-grabbed-server
   (lambda ()
     (cond ((X-property-get window name)
	    #f)
	   (#t
	    (X-property-set! window name value type format 'replace)
	    #t)))))

;; from Todd Larason
(define*-public (run-in-netscape command completion #&optional (netwin (netscape-win)))
  "Runs COMMAND in a Netscape window, calling COMPLETION when done, if set.
Uses Netscape window NETWIN if specifies, or one found by (netscape-win)
otherwise; it is an error if NETWIN refers to a non-Netscape window."
  (letrec
      ((get-mozilla-hook
	(lambda ()
	  (if (X-atomic-property-set-if-unset! netwin "_MOZILLA_LOCK" "lock!")
	      (begin
		(add-hook! X-PropertyNotify-hook mozilla-property-notify)
		(put-mozilla-command))
	      (add-timer-hook! 50 get-mozilla-hook))))

       (put-mozilla-command
	(lambda ()
	  (X-property-set! netwin "_MOZILLA_COMMAND" command)))

       (mozilla-property-notify
	(lambda (propname window)
	  (cond ((and (eq? window netwin)
		      (string=? propname "_MOZILLA_RESPONSE"))
		 (remove-hook! X-PropertyNotify-hook mozilla-property-notify)
		 (X-property-get netwin "_MOZILLA_LOCK" #t)
		 (if completion
		     (completion (car (X-property-get
				       netwin "_MOZILLA_RESPONSE" #t)))))))))
    (get-mozilla-hook)
    #t))

(define-public netscape-new-window
;;;**VAR
;;; If #t, `netscape-goto-cut-buffer-url' will open the URL in a new window.
  #f)

(define*-public (netscape-goto-cut-buffer-url
                #&optional (new netscape-new-window))
  "Make netscape go to the URL in CUT_BUFFER0.
This permits you to just select a URL and use this function
to go to that page.
The optional argument specifies whether a new window should be opened.
It defaults to `netscape-new-window'."
  (run-in-netscape
   (string-append "openURL(" (X-cut-buffer-string) (if new ",new-window)" ")"))
   display-message-briefly (netscape-win)))

;; (run-in-netscape "openUrl(http://vicarious-existence.mit.edu/scwm)" display-message-briefly)
;; (run-in-netscape "openUrl(http://www.cs.washington.edu/homes/gjb)" display-message-briefly)

;; Inspired by Julian Satchell's version of this --10/09/98 gjb
(define-public (use-change-desk-commands vector-of-commands)
  "Execute one of the VECTOR-OF-COMMANDS shell commands when the desk changes.
The 0th element of the vector is used for changes to desk 0,
the first element for changes to desk 1, etc.  Changes to desks which are
\"off the end\" of the vector do nothing."
  (add-hook! change-desk-hook
	     (lambda (new old)
	       ;; (display n) (newline) ;; for debugging
	       (if (< new (vector-length vector-of-commands))
		   (system (vector-ref vector-of-commands new)))
	       )))

(define-public (execute-on-selection command)
  "Run COMMAND in the background, with arguments supplied by the X selection."
  (execute (string-append command " '" (X-cut-buffer-string) "'")))

(define-public (exe-on-selection command)
  "Return a procedure that runs COMMAND in the background on the X selection."
  (lambda () (execute-on-selection command)))

(define (extreme1 pred lst)
  (if (null? (cdr lst))
      (car lst)
      (let ((ex (extreme pred (cdr lst))))
	(if (pred (car lst) ex)
	    (car lst)
	    ex))))

(define-public (extreme pred lst)
  "Find extreme value e of PRED in LST.
If PRED defines a semi-ordering, `(PRED e x)' will hold for all members x
of LST not equal to e. E.g. `(extreme < ...)' returns the lowest number."
  (if (null? lst)
      ()
      (extreme1 pred lst)))

(define*-public (take-screenshot
		 #&optional (template (string-append
				       (user-home)
				       "/screenshot%y%m%d%H%M%S.xwd")))
  "Take a snapshot of the whole screen.
The screenshot will be saved in xwd format in the filename constructed from
TEMPLATE. %-escapes in TEMPLATE will be replaced by time-elements, according
to strftime rules. TEMPLATE defaults to the file \"screenshot%y%m%d%H%M%S.xwd\"
in the user's home directory."
  (execute (string-append "xwd -root >"
			  (strftime template (localtime (current-time))))))


;;; palm pilot stuff
;;; requires pilot-link's pilot-clip program

;; (system "ssh-add </dev/null &")
;;(define pilot-clip-binary "pilot-clip")
(define pilot-clip-binary "remote-pilot-clip")  ;; this does ssh HOST-WITH-CRADLE pilot-clip "$@"

(define-public (put-string-in-palm-clipboard str)
  (let ((port (open-output-pipe (string-append pilot-clip-binary " -s &"))))
    (display str port)
    (close-port port)))

(define-public (X-cut-buffer->palm-clipboard) 
  (put-string-in-palm-clipboard (X-cut-buffer-string)))

;;(put-string-in-palm-clipboard "testing\nto\nsee\nif this\nworks")
;; (X-cut-buffer->palm-clipboard)

;; This is not such a hot idea-- scwm can hang!
;; (define-public (get-string-from-palm-clipboard)
;;   (let* ((port (open-input-pipe (string-append pilot-clip-binary " -g")))
;; 	    (str (read-line port)))
;;     (close-port port)
;;     str))
;; 
;; (get-string-from-palm-clipboard)

(define-public (delete-multiple-windows-interactively)
  (select-multiple-windows-interactively #f delete-window))

(define-public (run-dot-xclients-script)
  "Runs the ~/.clients script."
  (system "$HOME/.xclients &"))

(define-public (run-dot-xclients-at-startup)
  (add-hook! startup-hook (lambda () (if (not (restarted?)) (run-dot-xclients-script)))))

(define*-public (rename-window-interactively #&optional (win (get-window)))
  "Prompt for a new name for WIN and change its title.
WIN defaults as usual to the current window context."
  (string-prompt (string-append "Rename \"" (window-title win) "\" to: ") (lambda (new-name)
				 (set-window-title! win new-name))
		 "Rename-window"))


;; ((help-mesg "move-to"))

(define-public (read-until-eof in)
  "Return all the text from input port IN until eof.
IN should be a newline-terminated Ascii input port."
  (let ((l (read-line in))
	(answer ""))
    (while (not (eof-object? l))
	   (set! answer (string-append answer l "\n"))
	   (set! l (read-line in)))
    answer))

(define-public (output-of-system-cmd cmd)
  "Return the output of command shell execution of CMD.
CMD is run synchronously and its output is piped into the return value
of this function, as a string."
  (let* ((p (open-input-pipe cmd))
	 (answer (read-until-eof p)))
    (close-pipe p)
    answer))

(define-public (chop-newline str)
  "Return STR up to but not including the first newline character."
  (let ((ich (string-index str #\newline)))
    (if (not ich)
	str
	(substring str 0 ich))))

(define-public (group-leader-id win)
  (vector-ref (car (X-property-get win "WM_HINTS")) 8))

(define-public (group->windows group)
  (list-windows #:only (lambda (win) (= (group-leader-id group) (group-leader-id win)))))
