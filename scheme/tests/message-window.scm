;; $Id$

(use-modules (app scwm flux))

(display-message-briefly "foo")
(display-message-briefly "longer message for justification test")

(define mwn
  (make-message-window "foo"))

(make-message-window "msg")
(message-window-show! mwn)
(message-window-hide! mwn)


(message-window-set-message! mwn "New message")
(message-window-set-position! mwn 100 100 -.5 -.5)
(message-window-set-position! mwn 100 100 1 1)

;; (set! mwn #f)   ;; this will make it impossible to remove mwn, but should not dump core

;; images stuff

(define i (window->image (get-window)))
(define i2 (clone-scaled-image i 50 50))

(image-properties i2)

(define root (window->image 'root-window))

(clear-image-cache-entry "scwm-logo.xpm")
(define logo (make-image "scwm-logo.xpm"))
(define logo (make-image "scwmlogo.xpm"))

(image-properties i)
(image-properties logo)
(image-properties root)

(popup-menu (menu (list (menuitem "foo" #:image-above i))))

(define m (make-message-window-clone-default "Testing..."))
(define m (make-message-window-clone-default ""))
(define m (make-message-window-with-image (vector-ref logo-image-sequence 1) #t))

(message-window-message m)

(message-window-set-image! m i)
(message-window-set-image! m i2)
(message-window-set-image! m root)
(message-window-set-image! m logo)
(message-window-set-image! m logo (make-color "white") #f #t)
(message-window-set-image! m (vector-ref logo-image-sequence 2) #f #f #t)

(message-window-show! m)
(message-window-hide! m)
(message-window-hide! scwm-logo-msgwin)

(message-window-set-size! m 100 100)
(message-window-set-size! m 50 50)
(message-window-set-size! m #f #f)
(apply message-window-set-size! (cons m (image-size logo)))
(message-window-set-position! m (/ display-width 2) (/ display-height 2))

(message-window-set-colors! m "white" "gray50")

(message-window-set-relief! m #f)
(message-window-set-relief! m #t)

(message-window-set-message! m "Foo")
(message-window-set-message! m "")
(message-window-set-message! m "")
(message-window-set-size! m 100 100)
(message-window-set-size! m 500 500)

(message-window-set-position! m 900 150)


(define i (window->image 'root-window))
(define m (make-message-window ""))

(message-window-set-image! m i)
(image-properties i)

(use-scwm-modules message-window)

(begin
  (message-window-set-size! m 1280 1024)
  (message-window-set-position! m 0 0 0 0)
  (message-window-set-relief! m #f)
  (message-window-show! m)
  (add-timer-hook! (sec->usec 3) (lambda () (message-window-hide! m))))

(with-frozen-root-window
 (lambda ()
   (sleep 2)))
