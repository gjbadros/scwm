
(popup-menu
 (make-menu
  (list 
   (make-menuitem "Foo" 
		  (lambda () (display "foo\n"))
		  #f #f #f
		  (lambda () (display-message "foo hover"))
		  (lambda () (hide-message)))
   (make-menuitem "Bar" 
		  (lambda () (display "bar\n"))
		  #f #f #f
		  (lambda () (display-message "bar hover"))
		  (lambda () (hide-message))))))

(use-modules (app scwm flux))
(use-modules (app scwm optargs))
(use-modules (app scwm winlist))
(use-modules (app scwm doc))

(popup-menu
 (make-menu
  (let* ((wl (list-windows #:by-stacking #t))
	 (w1 (car wl))
	 (w2 (cadr wl)))
    (list 
     (make-menuitem "Window list" #f)
     (make-menuitem (window-title w1) (lambda () w1) #f #f #f
		    (lambda () (flash-window w1 #:unflash-delay #f))
		    (lambda () (unflash-window w1)))
     (make-menuitem (window-title w2) (lambda () w2) #f #f #f
		    (lambda () (flash-window w2 #:unflash-delay #f))
		    (lambda () (unflash-window w2)))))))

(flash-window (select-window-interactively) #:unflash-delay 10)
(unflash-window (select-window-interactively))

(object-property (select-window-interactively) 'old-bg)
(object-property (select-window-interactively) 'old)

(set-object-property! (select-window-interactively) 'old #t)


(move-viewport 0 0)

(set-X-server-synchronize! #t)

(set-viewport-position! 500 0)



