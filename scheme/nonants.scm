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
  :use-module (app scwm optargs))



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


(define-public (get-window-with-nonant-interactively)
  "Interactively select a window and a nonant.
The nonant is stored as an object-property of the window
for use with the window-selection and constraints modules."
  (let* ((selinf (select-viewport-position))
	 (win (car selinf)))
    (if (window? win)
	(let ((nonant (get-window-nonant selinf)))
	  (set-object-property! win 'nonant nonant)
	  win)
	#f)))


;; determining the nonant from select-viewport-position

(define-public (get-window-nonant select-list)
  "SELECT-LIST is a list of (win x y), returns the nonant selected.
The nonant is a number in [0,8] referring to which of the tic-tac-toe board
squares x,y is in of WIN. x,y are viewport positions.
`select-viewport-position' returns lists of the form needed by this procedure."
  (let* ((pos (window-viewport-position (car select-list)))
         (size (window-frame-size (car select-list)))
	 (dx (- (cadr select-list) (car pos)))
	 (dy (- (caddr select-list) (cadr pos)))
	 (qx (quotient dx (quotient (car size) 3)))
	 (qy (quotient dy (quotient (cadr size) 3))))
    (+ (* 3 qy) qx)))


;; nonant naming

(define nonant-names '("NW" "N" "NE" "W" "C" "E" "SW" "S" "SE"))

(define-public (nonant->string nonant)
  "Return the string name for NONANT."
  (list-ref nonant-names nonant))

;; (nonant->string 4)
