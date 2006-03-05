#!
!#


(window-gravity (car a))
(window-gravity (get-window))

(begin
  (define w1 (get-window))
  (define w2 (get-window))
  (define w3 (get-window))
  (list (window-gravity w1)
	(window-gravity w2)
	(window-gravity w3))
  )

(resize-frame 100 100 w1)
(resize-frame 200 200 w1)
(resize-frame 100 100 w2)
(resize-frame 200 200 w2)
(resize-frame 100 100 w3)
(resize-frame 200 200 w3)

(use-scwm-modules animation)

(animated-resize-frame 100 100 w1)
(animated-resize-frame 200 200 w1)
(animated-resize-frame 100 100 w2)
(animated-resize-frame 200 200 w2)
(animated-resize-frame 100 100 w3)
(animated-resize-frame 200 200 w3)



(resize-frame 100 100 w1 0 0)
(resize-frame 200 200 w1 0 0)
(resize-frame 100 100 w2 0 0)
(resize-frame 200 200 w2 0 0)
(resize-frame 100 100 w3 0 0)
(resize-frame 200 200 w3 0 0)

(use-scwm-modules animation)

(animated-resize-frame 100 100 w1 0 0)
(animated-resize-frame 200 200 w1 0 0)
(animated-resize-frame 100 100 w2 0 0)
(animated-resize-frame 200 200 w2 0 0)
(animated-resize-frame 100 100 w3 0 0)
(animated-resize-frame 200 200 w3 0 0)


(define* (little-top-left #:optional (win (get-window)))
  (animated-resize-frame 100 100 win 0 0))

(bind-key 'window "H-a" little-top-left)
(bind-key 'window "H-r" resize-halfscreen)
(animated-resize-frame 100 100 w2 0 0)
(animated-resize-frame 100 100 w3 0 0)
(resize-frame 100 100 w2 0 0)

(resize-frame 100 100 w1)
(resize-frame 100 100 w1)

(define-public (resize-halfscreen)
  "Resize the current window with the pointer to full height and half the screen size in width."
  (let ((w (current-window-with-pointer)))
    (animated-resize-window (%x 49) (%y 90))))


(hide-titlebar (get-window) #t)
(show-titlebar (get-window) #t)
