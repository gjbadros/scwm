;; keep two windows at same y coord
(begin
 (define w1 (get-window))
 (define w2 (get-window))
 (add-stays-on-window w1)
 (add-stays-on-window w2)
 (add-stays-on-window (current-window-with-focus)))

(add-stays-on-window (get-window))

(keep-tops-even (current-window-with-focus) w2)
(keep-to-left-of (current-window-with-focus) w2)
(keep-to-left-of (current-window-with-focus) (get-window))
(keep-to-left-of (current-window-with-focus) (get-window))

(define v (make-cl-variable))
(define e (make-cl-expression v))
(define eq (make-cl-equation e))
(define ineq (make-cl-inequality e))
(define solver (make-cl-solver))

(cl-variable? c)
(cl-variable? w1)
(cl-expression? e)
(cl-expression? w1)
(cl-equation? eq)
(cl-equation? w1)
(cl-inequality? ineq)
(cl-inequality? w1)
(cl-solver? solver)
(cl-solver? w1)


;; obsoleted stuff below here

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
