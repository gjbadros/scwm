;; $Id$ -*- scwm -*-
;; --07/25/98 gjb

(map (lambda (s) (select-window-interactively s))
     (list "A win"
	   "Another win"
	   "A really really really long window name"
	   "A really really really long window name that is even longer because it just contains junk lots and lots of junk and lots of junk junk junk "))


(define (move-to-top-left-desk win)
  (let* ((disp-size (display-size))
	 (disp-width (car disp-size))
	 (disp-height (cadr disp-size))
	 (oldpos (window-position win))
	 (old-x (car oldpos))
	 (old-y (cadr oldpos))
	 (new-x (modulo old-x disp-width))
	 (new-y (modulo old-y disp-height)))
    (move-to new-x new-y win))) 
;;    (for-each (lambda (e) (display e))
;;		(list 
;;		 "old pos = " old-x "," old-y "\n"
;;		 "new pos = " new-x "," new-y "\n"))))


;;(move-to-top-left-desk (select-window-interactively))

(for-each (lambda (w) (move-to-top-left-desk w)) (list-all-windows))
	       
		 
