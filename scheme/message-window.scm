;; $Id$
;; Move/Resize w/ Message Window
;; By Jeffrey Nichols
;;
;; A scheme implementation of the message window that appears 
;; and tells you how much you're moving/resizing your window.


(define-module (app scwm message-window)
  :use-module (app scwm base)
  :use-module (app scwm optargs))


;;; positioning a message window

(define-public (gravity->alignments gravity)
  "Return a list of two numeric alignments corresponding to a GRAVITY.
GRAVITY can be one of 'nw, 'n, 'ne, 'w, 'center, 'e, 'sw, 's, 'se
(or spelled-out versions of these)."
   (case gravity
     ((nw northwest north-west) '(  0    0))
     ((n  north)                '(-.5    0))
     ((ne northeast north-east) '( -1    0))
     ((w  west)                 '(  0  -.5))
     ((center)                  '(-.5  -.5))
     ((e  east)                 '( -1  -.5))
     ((sw southwest south-west) '(  0   -1))
     ((s  south)                '(-.5   -1))
     ((se southeast south-east) '( -1   -1))
     (else (error "Invalid gravity specified."))))


(define-public (position-message-window! msgwin x y gravity)
  "Move the move/resize message window's GRAVITY point to (X,Y).
GRAVITY can be one of 'nw, 'n, 'ne, 'w, 'center, 'e, 'sw, 's, 'se
(see `gravity->alignments')."
  (apply
   (lambda (xa ya)
     (message-window-set-position! msgwin x y xa ya))
   (gravity->alignments gravity)))


;;;; message-window-options - configure placement and behavior for various actions

(define*-public (message-window-options action #&key enable
					follow-window 
					position gravity offset 
					proportional-offset)
  "Allow specification of message-window behavior for standard actions.

ACTION specifies the action for which this behavior should be
used. Currently, 'interactive-move and 'interactive-resize are the
only useful values. 

ENABLE is a boolean value inidicating whether to use a message window
for that action at all.

GRAVITY indicates which point on the message window is used as the
control point for placing it. See `gravity->alignmnents' for a list of
valid gravity specs.

If FOLLOW-WINDOW is true, the message window will track the window on
which an action is being performed, if applicable. In this case,
POSITION is ignored; OFFSET is taken as a list of the X and Y offsets
from the window's top left corner, and PROPORTIONAL-OFFSET is taken as
a list of factors to multiply by the window's size. For instance, an
OFFSET of (0 0) and a PROPORTIONAL-OFFSET of (0.5 0.5) will place the
message window at the center of of the window being operated on.

However, if FOLLOW-WINDOW is false, or there is no applicable window,
POSITION is interpreted as a list of x, y coordinates at which to
place the message window.

These options are cumulative for repeated calls to
`message-window-options' for the same action."
  (let ((options (or (msgwin-action-options action)
		     (copy-tree default-msgwin-options))))
    (if (bound? enable)
	(message-window-enable action enable))
    (if (bound? follow-window)
	(assq-set! options 'follow-window follow-window))
    (if (bound? position)
	(assq-set! options 'position position))
    (if (bound? gravity)
	(assq-set! options 'gravity gravity))
    (if (bound? offset)
	(assq-set! options 'offset offset))
    (if (bound? proportional-offset)
	(assq-set! options 'proportional-offset proportional-offset))
    (set-msgwin-action-options! action options)))

(define (msgwin-placer optlist)
  (let* ((follow-window (assq-ref optlist 'follow-window))
	 (position (assq-ref optlist 'position))
	 (pos-x (car position))
	 (pos-y (cadr position))
	 (gravity (assq-ref optlist 'gravity))
	 (offset (assq-ref optlist 'offset))
	 (offset-x (car offset))
	 (offset-y (cadr offset))
	 (proportional-offset (assq-ref optlist 'proportional-offset))
	 (po-x (car proportional-offset))
	 (po-y (cadr proportional-offset)))
    (lambda (msgwin win)
      (if (and follow-window win)
	  (let* ((pos (window-viewport-position win))
		 (size (window-frame-size win))
		 (x (+ (car pos) offset-x (round (* po-x (car size)))))
		 (y (+ (cadr pos) offset-y (round (* po-y (cadr size))))))
	    (position-message-window! msgwin x y gravity))
	  (position-message-window! msgwin pos-x pos-y gravity)))))

(define default-msgwin-options
  `((follow-window . #f) (position . (,(%x 50) ,(%y 50))) 
			 (gravity . center) (offset . (0 0)) 
			 (proportional-offset . (0.5 0.5))))

(define msgwin-option-table (make-vector 3 #f))
(define msgwin-placer-table (make-vector 3 #f))


(define (msgwin-action-options action)
  (hashq-ref msgwin-option-table action))

(define (msgwin-action-placer action)
  (hashq-ref msgwin-placer-table action))

(define (set-msgwin-action-options! action options)
  (hashq-set! msgwin-option-table action options)
  (hashq-set! msgwin-placer-table action (msgwin-placer options)))


;;;; the default message window

(define-public default-message-window (make-message-window ""))

;;; hooks for the usual resize and move message windows

(define (enable-resize-msgwin)
  (add-hook! interactive-resize-start-hook resize-message-start-hook)
  (add-hook! interactive-resize-new-size-hook resize-message-new-size-hook)
  (add-hook! interactive-resize-finish-hook resize-message-finish-hook))

(define (disable-resize-msgwin)
  (remove-hook! interactive-resize-start-hook resize-message-start-hook)
  (remove-hook! interactive-resize-new-size-hook resize-message-new-size-hook)
  (remove-hook! interactive-resize-finish-hook resize-message-finish-hook))

(define (enable-move-msgwin)
  (add-hook! interactive-move-start-hook move-message-start-hook)
  (add-hook! interactive-move-new-position-hook move-message-new-position-hook)
  (add-hook! interactive-move-finish-hook move-message-finish-hook))

(define (disable-move-msgwin)
  (remove-hook! interactive-move-start-hook move-message-start-hook)
  (remove-hook! interactive-move-new-position-hook 
		move-message-new-position-hook)
  (remove-hook! interactive-move-finish-hook move-message-finish-hook))

(define msgwin-enabler-table (make-vector 3))
(define msgwin-disabler-table (make-vector 3))

(hashq-set! msgwin-enabler-table 'interactive-move enable-move-msgwin)
(hashq-set! msgwin-disabler-table 'interactive-move disable-move-msgwin)
(hashq-set! msgwin-enabler-table 'interactive-resize enable-resize-msgwin)
(hashq-set! msgwin-disabler-table 'interactive-resize disable-resize-msgwin)


(define (message-window-enable action enable)
  (cond 
   ((hashq-ref (if enable 
		   msgwin-enabler-table 
		   msgwin-disabler-table) action) => (lambda (x) (x)))))

(define (resize-message-start-hook win width height)
    ((msgwin-action-placer 'interactive-resize) default-message-window win)
    (message-window-show! default-message-window))

(define (resize-message-new-size-hook win width height)
  (let* ((wstr (number->string width))
	 (hstr (number->string height))
	 (str (string-append " " wstr " x " hstr " ")))
    ((msgwin-action-placer 'interactive-resize) default-message-window win)
    (message-window-set-message! default-message-window str)))

(define (resize-message-finish-hook win)
    (message-window-hide! default-message-window))

(define (move-message-start-hook win)
  ((msgwin-action-placer 'interactive-move) default-message-window win)
  (message-window-show! default-message-window))

(define (move-message-new-position-hook win x y)
  (let* ((xstr (number->string x))
	 (ystr (number->string y))
	 (xstr (if (> 0 x) xstr (string-append "+" xstr)))
	 (ystr (if (> 0 y) ystr (string-append "+" ystr)))
	 (str (string-append " " xstr " " ystr " ")))
    ((msgwin-action-placer 'interactive-move) default-message-window win)
    (message-window-set-message! default-message-window str)))

(define (move-message-finish-hook win)
  (message-window-hide! default-message-window))

(define*-public (message-window-style msgwin #&key (font #f) (fg #f) (bg #f))
  "Set visual style of MSGWIN.
FONT specifies the font, FG the foreground color, and BG the
background color."
  (if font
      (message-window-set-font! msgwin font))
  (message-window-set-colors! msgwin fg bg))


;;; enable move and resize message windows by default

(message-window-options 'interactive-resize #:enable #t)
(message-window-options 'interactive-move #:enable #t)
