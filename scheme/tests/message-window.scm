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

