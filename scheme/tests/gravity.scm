(window-gravity (car a))
(window-gravity (get-window))

(define w1 (get-window))
(define w2 (get-window))

(animated-resize-frame 100 100 w1 0 0)
(animated-resize-frame 100 100 w2 0 0)
(resize-frame 100 100 w2 0 0)

(resize-frame 100 100 w1)
(resize-frame 100 100 w1)
