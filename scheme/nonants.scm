;;; $Id$
;;; nonants.scm
;;; Copyright (C) 1999 Jeff W. Nichols
;;;
;;; Functions for creating and handling window nonants
;;;
;;; Nonants are a division of a window into nine equal parts
;;; like so:
;;;
;;;    _____________
;;;    | 0 | 1 | 2 |
;;;    -------------
;;;    | 3 | 4 | 5 |
;;;    -------------
;;;    | 6 | 7 | 8 |
;;;    -------------
;;;
;;; This division is used by several ui-constraints-classes
;;; to create a more intuitive interface to building constraints.
;;;



(define-module (app scwm nonants)
  :use-module (app scwm base)
  :use-module (app scwm defoption)
  :use-module (app scwm message-window)
  :use-module (app scwm highlight-current-window)
  :use-module (app scwm optargs))




(defmacro-public with-motion-handler (proc . body)
;;;Use PROC as a motion handler while evaluating BODY
  `(let ((mh-proc ,proc))
     (dynamic-wind
      (lambda () (add-motion-handler! mh-proc))
      (lambda () ,@body)
      (lambda () (remove-motion-handler! mh-proc)))))

;; selecting nonants from windows

(define*-public (get-window-with-nonant #&optional (window #f) (nonant 4))
  "Select a WINDOW and a NONANT, defaulting to the current window context.  
If the WINDOW is not specified and there is no window context, 
perform interactive selection.  If WINDOW is passed in but 
NONANT is not specified, the center nonant is used.  The nonant
is stored as an object-property of the window for use with the
window-selection and constraints modules."
  (if (or window (window-context))
      (begin (set-object-property! window 'nonant nonant) (or window (window-context)))
      (get-window-with-nonant-interactively)))

(define (motion-handler-debug x_root y_root state win dx dy)
  (for-each (lambda (v) (display v) (display " "))
	    (list x_root y_root state win dx dy))
  (if win
      (display (nonant->string (window-and-offsets->nonant win dx dy))))
  (newline))

(define interactive-mark-nonant-msgwin
  (let ((img (make-image-or-warn "anchor.xpm")))
    (if img
	(make-message-window-with-image img)
	(make-message-window-clone-default "<>"))))

;;(message-window-show! interactive-mark-nonant-msgwin)
;;(message-window-hide! interactive-mark-nonant-msgwin)

(define-public (nonant-decoration win nonant)
  "Return the window id numbers of the decoration for WIN corresponding to NONANT.
E.g., NONANT == 0 will answer the id of the northwest corner window."
  (let* ((decoration-ids (window-decoration-ids win))
	 (sides (caddr decoration-ids))
	 (corners (cadddr decoration-ids)))
    (case nonant
      ((0 northwest nw) (list-ref corners 0))
      ((2 northeast ne) (list-ref corners 1))
      ((8 southeast se) (list-ref corners 2))
      ((6 southwest sw) (list-ref corners 3))
      ((1 north n) (list-ref sides 0))
      ((5 east e) (list-ref sides 1))
      ((7 south s) (list-ref sides 2))
      ((3 west w) (list-ref sides 3))
      ((4 center) #f)
      (else (error "Use a nonant number in [0,8] or a compass direction!")))))


(define-scwm-option *nonant-highlight-color* (make-color "green")
  "The color to use when highlighting window nonants during selection."
  #:type 'color
  #:group 'winops
  #:setter (lambda (c) (set-nonant-highlight-color! c))
  #:getter (lambda () (nonant-highlight-color)))

(define lastwin #f)

(define (mark-nonant-motion-handler x_root y_root state win dx dy)
  (if win 
      (begin
	(set-window-highlighted-nonant! 
	 (window-and-offsets->nonant win dx dy) win)))
  (if (and lastwin (not (eq? win lastwin)))
      (set-window-highlighted-nonant! #f lastwin))
  (set! lastwin win))

;; (reset-motion-handlers!)
;; nonants
(define-public (get-window-with-nonant-interactively)
  "Interactively select a window and a nonant.
The nonant is stored as an object-property of the window
for use with the window-selection and constraints modules."
  (dynamic-wind
   (lambda ()
     (start-highlighting-selected-window)
     (add-motion-handler! mark-nonant-motion-handler))
;;     (add-motion-handler! motion-handler-debug))
   (lambda ()
     (let* ((selinf (select-viewport-position))
	    (win (car selinf)))
       (if (window? win)
	   (let ((nonant (get-window-nonant selinf)))
	     (set-object-property! win 'nonant nonant)
	     win)
	   #f)))
   (lambda ()
     (end-highlighting-selected-window)
     (set-window-highlighted-nonant! #f lastwin)
     (remove-motion-handler! mark-nonant-motion-handler))))
;;     (remove-motion-handler! motion-handler-debug))))


;; determining the nonant from select-viewport-position

(define-public (window-and-offsets->nonant win dx dy)
  "Return a nonant number in [0,8] from a window position and an offset.
WIN is the window, DX and DY are positions relative to the top-left of WIN."
  (let* ((size (window-frame-size win))
	 (qx (quotient dx (quotient (car size) 3)))
	 (qy (quotient dy (quotient (cadr size) 3)))
	 (answer (+ (* 3 qy) qx)))
    (if (< answer 0) 0 (if (> answer 8) 8 answer))))

(define-public (get-window-nonant select-list)
  "SELECT-LIST is a list of (win x y), returns the nonant selected.
The nonant is a number in [0,8] referring to which of the tic-tac-toe board
squares x,y is in of WIN. x,y are root-window relative viewport positions.
`select-viewport-position' returns lists of the form needed by this procedure."
  (let* ((win (car select-list))
	 (x (cadr select-list))
	 (y (caddr select-list))
	 (pos (window-viewport-position win))
	 (dx (- x (car pos)))
	 (dy (- y (cadr pos))))
    (window-and-offsets->nonant win dx dy)))


;; nonant naming

(define nonant-names '("NW" "N" "NE" "W" "C" "E" "SW" "S" "SE"))

(define-public (nonant->string nonant)
  "Return the brief string name for NONANT, an integer.
E.g., an argument of 1 returns `N'."
  (list-ref nonant-names nonant))

;; (nonant->string 4)
;; (window-and-offsets->nonant w 700 300)
