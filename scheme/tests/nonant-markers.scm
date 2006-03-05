
(use-scwm-modules window-locations optargs flash-window)
(define w (get-window))

(map (lambda (v) 
       (set-window-highlighted-nonant! v w)
       (eq? v (window-highlighted-nonant w))
       (sleep 1))
     '(0 1 2 3 4 5 6 7 8 left hcenter right top vmiddle bottom))
(set-window-highlighted-nonant! 'right w)
(window-highlighted-nonant w)

(define (get-window-with-nonant)
  (let* ((selinf (select-viewport-position))
	 (win (car selinf)))
    (if (window? win)
	(let ((nonant (get-window-nonant selinf)))
	  (set-object-property! win 'nonant nonant)
	  win)
	#f)))

(define (set-markwin-offset! win nonant markwin)
  (let* ((marksize (message-window-size markwin))
	 (winsize  (window-frame-size win))
	 (winpos   (window-viewport-position win))
	 (xoffset  (round (* 0.3 (car winsize))))
	 (yoffset  (round (* 0.3 (cadr winsize))))
	 (xnon     (- (remainder nonant 3) 1))
	 (ynon     (- (quotient nonant 3) 1))
	 (xpos     (+ (car winpos) (quotient (car winsize) 2)))
	 (ypox     (+ (cadr winpos) (quotient (cadr winsize) 2))))
    (message-window-set-position! markwin (+ xpos (* xoffset xnon)) (+ ypox (* yoffset ynon)))))


(define* (place-nonant-marker #:optional (w (get-window-with-nonant)))
  (if (and (window? w) (object-property w 'nonant))
      (let ((nonant (object-property w 'nonant))
	    (markwin (if (message-window? (object-property w 'markwin))
			 (object-property w 'markwin)
			 (make-message-window "*"))))
	(set-markwin-offset! w nonant markwin)
	(message-window-show! markwin)
	(set-object-property! w 'markwin markwin))))

	     
(define* (remove-nonant-marker #:optional (w (get-window)))
  (let ((markwin (object-property w 'markwin)))
    (if (message-window? markwin)
	(begin 
	  (set-object-property! w 'markwin #f)
	  (message-window-hide! markwin)))))


(define (reset-position w)
  (let ((markwin (object-property w 'markwin))
	(nonant  (object-property w 'nonant)))
    (if (message-window? markwin)
	(set-markwin-offset! w nonant markwin))))

(define (change-hook x y dx dy)
  (for-each reset-position markwin-list))

(add-hook! viewport-position-change-hook change-hook)
(remove-hook! viewport-position-change-hook change-hook)      

(define (move-hook win new-x new-y) 
  (reset-position win))

(add-hook! interactive-move-new-position-hook move-hook)
(remove-hook! interactive-move-new-position-hook move-hook)

(define (resize-hook win x y new-w new-h new-wu new-hu) 
  (reset-position win))

(add-hook! interactive-resize-new-size-hook resize-hook)
(remove-hook! interactive-resize-new-size-hook resize-hook)

(define (desk-hook new old)
  (for-each (lambda (w)
	      (let ((desk (window-desk w))
		    (markwin (object-property w 'markwin)))
		(if (message-window? markwin)
		    (if (eqv? desk new)
			(message-window-show! markwin)
			(message-window-hide! markwin)))))
	      markwin-list))
  
(add-hook! change-desk-hook desk-hook)
(remove-hook! change-desk-hook desk-hook)
