;; $Id$
;; Move w/ Message Window
;; By Jeffrey Nichols
;;
;; A scheme implementation of the message window that appears 
;; and tells you where you're moving your window.

(define-module (app scwm move-with-message-window)
  :use-module (app scwm move-resize-message-window))

(add-hook! interactive-move-start-hook
	   (lambda (win)
	     (message-window-show! move-resize-message-window)))

(add-hook! interactive-move-new-position-hook
	   (lambda (win x y)
	     (let* ((xstr (number->string x))
		    (ystr (number->string y))
		    (xstr (if (> 0 x) xstr (string-append "+" xstr)))
		    (ystr (if (> 0 y) ystr (string-append "+" ystr)))
		    (str (string-append " " xstr " " ystr " ")))
	       (message-window-set-message! move-resize-message-window str))))

(add-hook! interactive-move-finish-hook
	   (lambda (win)
	     (message-window-hide! move-resize-message-window)))
