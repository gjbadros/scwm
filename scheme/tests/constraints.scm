;; keep two windows at same y coord
(define w2 (get-window))
(keep-tops-even (current-window-with-focus) w2)

;; can use window-list menu to see positions easily, but this works too
(window-position (current-window-with-focus))
(window-position w)

(define (reposition-window w)
  (let ((wp (window-position w)))
    (move-to (car wp) (cadr wp) w)))

(define (reposition-all-windows)
  (map (lambda (w)
	 (reposition-window w))
       (list-windows)))

(define (write-all-windows)
  (map (lambda (w)
	 (write w))
       (list-windows)))

(define w1 (car (list-windows)))
(define w2 (cadr (list-windows)))

(reposition-window w1)
(reposition-window w2)

(define (my-interactive-move w)
  (interactive-move w)
  (reposition-all-windows))

(bind-mouse 'all "C-S-M-2" my-interactive-move)



;;; Local Variables:
;;; eval: (load "scwm")
;;; eval: (scwm-mode)
;;; End:

