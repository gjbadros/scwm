;;; $Id$
;;; flux.scm
;;; Copyright (C) 1998-1999 Sam Steingold, Greg J. Badros, and Maciej Stachowiak
;;;
;;; This are functions used by various sample .scwmrc, but not necessarily
;;; stabilized even as well as the other files in scheme/*.scm
;;; Expect the semantics of these functions to change, and don't
;;; be surprised if some even completely disappear (as we figure out a better
;;; way to do things)



(define-module (app scwm flux)
  :use-module (ice-9 regex)
  :use-module (app scwm base)
  :use-module (app scwm time-convert)
  :use-module (app scwm defoption)
  :use-module (app scwm wininfo)
  :use-module (app scwm winlist)
  :use-module (app scwm message-window)
  :use-module (app scwm winops)
  :use-module (app scwm flash-window)
  :use-module (app scwm listops)
  :use-module (app scwm file)
  :use-module (app scwm stringops)
  :use-module (app scwm group)
  :use-module (app scwm path-cache)
  :use-module (app scwm window-selection)
  :use-module (app scwm optargs)
  :use-module (app scwm xprop-extras))



(define-public user-init-file (string-append (user-home) "/.scwmrc"))

;; The #t arguments should perhaps instead be a closure
;; returning whether an opaque move/resize is desired

(define-public (interactive-move-window-with-focus)
  "Interactively move the window which currently has the focus.
`*move-opaquely-proc*' is used to control whether a rubberband
outline or the window itself is moved."
  (let ((w (current-window-with-focus))) (and w (interactive-move w))))

(define-public (interactive-resize-window-with-focus)
  "Interactively resize the window which currently has the focus.
`*resize-opaquely-proc*' is used to control whether a rubberband
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

(define-public (system-info-string)
  "Return a string with various system information.
Use `show-system-info' to display it in a window."
  (let ((vv (X-version-information)) (dd (X-display-information)))
    (apply
     to-string "Guile verion:\t\t" (version)
     "\nLibguile timestamp:\t" (libguile-config-stamp)
     "\nSCWM version:\t\t" (scwm-version)
     "\nFrom repository date:\t" (scwm-version-date)
     "\nRestarted:\t\t" (bool->string (restarted?))
     "\nDisplay Size:\t\t" (size->string (display-size))
     "\nDesk Size:\t\t" (size->string (desk-size))
     "\nViewport Position:\t" (size->string (viewport-position))
     "\nPointer:\t\t" (size->string (pointer-position))
     "\nCurrent Desk:\t\t" (number->string (current-desk))
     "\nX vendor:\t\t" (caddr vv) "; version: " (number->string (car vv)) "."
     (number->string (cadr vv)) "; release: " (number->string (cadddr vv))
     "\nX Display:\n\tResolution:\t" (size->string dd) "\n\tColor:\t\t"
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

;;; GJB:FIXME:: do not use xmessage-- use guile-gtk
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

(define*-public (window-info #&optional (win (get-window)))
  "Display information about WIN in a message window."
  (message
   "Window ID:\t\t" (number->string (window-id win))
   "\nWindow Frame ID:\t" (number->string (window-frame-id win))
   "\nTitle:\t\t\t\"" (window-title win) "\""
   "\nVirtual Position:\t\t" (size->string (window-position win))
   "\nViewport Position:\t\t" (size->string (window-viewport-position win))
   "\nSize:\t\t\t" (size->string (window-frame-size win))
   "\nDesk:\t\t\t" (number->string (window-desk win)) "\nClass:\t\t\t\""
   (window-class win) "\"\nResource:\t\t\"" (window-resource win)
   "\"\nBorder Normal:\t\t" (bool->string (border-normal? win))
   "\nFocus:\t\t\t" (get-window-focus win)
   "\nDeletable:\t\t" (bool->string (window-deletable? win))
   "\nIconified:\t\t" (bool->string (iconified? win))
   "\nKept On Top:\t\t" (bool->string (kept-on-top? win))
   "\nTransient:\t\t" (bool->string (transient? win))
   "\nRaised:\t\t\t" (bool->string (raised? win))
   "\nShaded:\t\t\t" (bool->string (window-shaded? win))
   "\nShaped:\t\t\t" (bool->string (window-shaped? win))
   "\nIcon Shaped:\t\t" (bool->string (window-icon-shaped? win))
   "\nSticky Icon:\t\t" (bool->string (icon-sticky? win))
   "\nSticky:\t\t\t" (bool->string (sticky? win))
   "\nTitle Bar Shown:\t" (bool->string (titlebar-shown? win))))

(define-public (show-system-info)
  "Display the `system-info-string' system details in a window."
  (message (system-info-string)))

(define (first-word s)
  "Return the first word of S (up to but not including first space char)."
  (let ((i (string-index s #\space))
	(j (string-index s #\tab))
	(l (string-length s)))
    (let ((k (if (< (or i j l) (or j i l)) i j)))
      (if k (substring s 0 k) s))))
;;(first-word "foo bar") => "foo"
;;(first-word "foo	bar") => "foo"
;;(first-word "foobar") => "foo"

(define-public (make-menuitems-from-menu-information-list menu-info-list)
  "Return a list of menu-items from a list of detailed programs list.
The format is subject to change.  See sample.scwmrc/gjb.scwmrc for
example usage."
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
	     menu-info-list)))


(define-public (sleep-ms ms)
  "Delay for MS milliseconds.
Note that timer-hooks are much more useful in nearly all
cases.  See `add-timer-hook!'."
  (select '() '() '() 0 (* 1000 ms)))


(define-public (select-window-group)
  "Prompt for multiple windows and return the list of selected windows.
Windows are highlighted (see `flash-window') as they are selected.  The
returned list can be used to un-highlight the windows:
 (let ((winlist (select-window-group)))
   (for-each (lambda (w) (unflash-window w)) winlist))"
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
;; (object-properties (select-window-interactively))
;; (for-each (lambda (w) (unflash-window w)) (list-all-windows))
;; (unflash-window)
;; (flash-window-on)



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
    (ap menu-separator)
    (do ((l (read-line p 'trim) (read-line p 'trim)))
	((eof-object? l) ret)
      (cond ((string-match "([^ \t]+)[ \t]+([^ \t]+)" l)
	     => (lambda (m)
		  (ap (mm (match:substring m 1)   ; machine name
			  (match:substring m 2))) ; user name
		  ))))
    (ap menu-separator)
    (ap (menuitem "reread .rhosts file" #:action
	    (lambda () (set! rhosts-menu (make-rhosts-menu)))))
    (menu ret)
)))

;; sds: users should call this function themselves
;;(define-public rhosts-menu (make-rhosts-menu))

(define-public (close-all-xlogo-windows)
  "Close each window with class == XLogo.
Greg uses XLogo windows as a sample window, so this
is useful for clearing the xlogos away when there get to
be more than desired."
  (for-each (lambda (w) (close-window w))
	    (list-windows #:only
			  (lambda (w)
			    (string=? (window-class w) "XLogo")))))


;; useful for debugging/testing
;;(set-X-server-synchronize! #t)




;; (run-in-netscape "openUrl(http://serveuse.mit.edu/scwm)" display-message-briefly)
;; (run-in-netscape "openUrl(http://www.cs.washington.edu/homes/gjb)" display-message-briefly)
;; (netscape-goto-url "http://serveuse.mit.edu/scwm")

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

;; ((help-mesg "move-to"))

(define-public (move-nonsticky-windows-relative x y)
  "Move all windows right X, down Y pixels.
See `move-window-relative.'"
  (for-each (lambda (w) (move-window-relative x y w)) (list-windows #:only (win-not?? sticky?))))

;; Recapture sometimes requires me to use this -- WHY? GJB:FIXME::
;; (use-modules (app scwm flux))
;; (move-nonsticky-windows-relative 0 display-height)
;; (move-nonsticky-windows-relative 0 (- display-height))
;; (move-nonsticky-windows-relative 0 -1)
;; (move-nonsticky-windows-relative display-width 0)
;; (move-nonsticky-windows-relative 1 0)
;; (move-nonsticky-windows-relative (- 0 display-width) 0)

(defmacro-public @ args
  `(lambda (sym)
     (variable-ref (module-variable (resolve-module ',args) sym))))

(define*-public (show-X-properties #&optional (win (get-window)))
  "Displays the X properties of WIN in a message window.
WIN is a window object, an X window id, or 'root-window."
  (message (X-properties->string win)))

;; (use-scwm-modules listops)

;;; window-corners, enclosing-rectangle, tile-windows-interactively
;;; By Greg J. Badros --07/04/99 gjb
;;; Inspired by code suggested by Todd Larason -- 17-Apr-1999
;; (window-corners (get-window))
(define-public (window-corners win)
  "Return the four coordinates of the corners of the location of WIN.
Return value's car is the top left, cdr is the bottom right.
That is, the returned list is: ((nw-x nw-y) se-x se-y).  Use
`rect-nw-x', `rect-nw-y', `rect-se-x', `rect-se-y' to take apart
the returned list."
  (let ((p (window-position win))
	(s (window-size win)))
    (set-cdr! (cdr s) ())
    (cons p (map + p s))))

(define-public rect-nw car)
(define-public rect-se cdr)
(define-public rect-nw-x caar)
(define-public rect-nw-y cadar)
(define-public rect-se-x cadr)
(define-public rect-se-y caddr)

;; (enclosing-rectangle l)
(define-public (enclosing-rectangle wins)
  "Return the smallest rectangle that encloses the windows WINS.
Return value's car is the top left of the rectangle, cdr is
the bottom right.
That is, the returned list is: ((nw-x nw-y) se-x se-y)."
  (let ((window-corners (map window-corners wins)))
    (let ((x1 (apply min (map rect-nw-x window-corners)))
	  (y1 (apply min (map rect-nw-y window-corners)))
	  (x2 (apply max (map rect-se-x window-corners)))
	  (y2 (apply max (map rect-se-y window-corners))))
      (cons (list x1 y1) (list x2 y2)))))

;;(tile-windows-interactively)
;;(tile-windows-interactively 'horizontal)
(define*-public (tile-windows-interactively #&optional (order 'vertical))
  "Tile a set of selected windows either vertically or horizontally based on ORDER.
The windows used are selected either by `selected-windows-list' or `select-window-group'.
If `selected-windows-list' is empty, then `select-window-group' is used.
See also the undo module and `insert-undo-global' to save the window 
configuration before executing this in case the effect is not what you
expected."
  (let* ((winlist (selected-windows-list))
	 (wins (if (pair? winlist) winlist (select-window-group)))
	 (r (enclosing-rectangle wins)))
    (if (pair? winlist)
	(unselect-all-windows)
	(for-each unflash-window wins))
    (tile-windows wins
		  #:start-pos (rect-nw r)
		  #:end-pos (rect-se r)
		  #:order order)))

