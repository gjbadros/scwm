;; wiggle-window just demonstrates 
(define (wiggle-window)
  (let ((w (get-window)))
    (window-shade w #t)
    (un-window-shade w #t)))

