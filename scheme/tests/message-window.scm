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
(define root (window->image 'root-window))

(image-properties i)
(image-properties root)

(popup-menu (menu (list (menuitem "foo" #:image-above i))))

(define m (make-message-window-clone-default "Testing..."))
(define m (make-message-window-clone-default "")

(message-window-message m)

(message-window-set-image! m i)
(message-window-set-image! m root)

(message-window-show! m)
(message-window-hide! m)

(message-window-set-size! m 100 100)
(message-window-set-size! m #f #f)

(message-window-set-relief! m #f)

(message-window-set-message! m "Foo")
(message-window-set-message! m "")
(message-window-set-message! m "")
(message-window-set-size! m 100 100)
(message-window-set-size! m 500 500)

(message-window-set-position! m 20 20)
