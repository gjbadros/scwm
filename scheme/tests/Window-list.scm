;; $Id$ -*- scwm -*-

(define w (current-window-with-focus))
(list-windows #:only (lambda (w) (string=? (window-class w) "XTerm")))

(define (netscape-win)
  (car 
   (list-windows #:only 
		 (lambda (w) 
		   (and (string=? (window-class w) "Netscape")
			(string=? (window-resource w) "Navigator"))))))

(define (move-next-to-netscape-win w)
  (let* ((ns-pos (window-position (netscape-win)))
	 (x (+ (car ns-pos) 300))
	 (y (- (cadr ns-pos) 30)))
    (move-to x y w #f #t)))

(move-next-to-netscape-win (select-window-interactively))
