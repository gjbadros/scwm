;; $Id$
;; Resize w/ Message Window
;; By Jeffrey Nichols
;;
;; A scheme implementation of the message window that appears 
;; and tells you how much you're resizing your window.

(define-module (app scwm resize-with-message-window)
  :use-module (app scwm move-resize-message-window))

(add-hook! interactive-resize-start-hook
	   (lambda (win x y)
	     (message-window-show! move-resize-message-window)))

(add-hook! interactive-resize-new-size-hook
	   (lambda (win width height)
	     (let* ((size (window-size win))
		    (w (caddr size))
		    (h (cadddr size))
		    (wstr (number->string w))
		    (hstr (number->string h))
		    (str (string-append " " wstr " x " hstr " ")))
	       (message-window-set-message! move-resize-message-window str))))

(add-hook! interactive-resize-finish-hook
	   (lambda (win)
	     (message-window-hide! move-resize-message-window)))
