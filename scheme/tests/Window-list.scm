;; $Id$ -*- scwm -*-

(define w (current-window-with-focus))

(map (lambda (w) (move-to 0 0 w)) (list-windows))
(list-windows #:only (lambda (w) (string=? (window-class w) "XTerm")))
(show-window-list-menu #:only (lambda (w) (string=? (window-class w) "XTerm")))

(window-resource (select-window-interactively))
(window-title (select-window-interactively))
(window-class (select-window-interactively))

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

(select-window-interactively)

;; Returns them in reverse the order they were selected
;; should probably turn off the invalid interaction hook
;; or provide a way of telling select-window-interactively that
;; the root window is not an erro
(define*-public (select-multiple-windows-interactively #:optional (max 32000))
  (do ((w '())
       (wlist '() (cons w wlist))
       (i 0 (+ 1 i)))
      ((or (not w) (>= i max)) 
       (if w wlist
	   (cdr wlist)))
    (set! w (select-window-interactively (string-append "select #" (number->string i))))))

(select-multiple-windows-interactively 10)

;; popup-menu needs to return the value of the procedure it executes
(define (select-window-from-window-list)
  (show-window-list-menu #:proc (lambda (w) w)))


(restack-windows (select-multiple-windows-interactively 3))
