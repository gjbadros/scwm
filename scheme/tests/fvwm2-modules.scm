;; $Id$

(use-scwm-modules fvwm-module module-types bincomm fvwm-compat fvwm-eval)

(fvwm2-module-send-window-list (current-output-port))

(define port (current-output-port))
(define body "foo")
(define str "foo")
(define id M_NEW_PAGE)

(define body-length .75)

(uniform-array-write "foo" (current-output-port))
(current-input-port)
(current-error-port)

(array? "foo")


(eval-fvwm-command "Send_WindowList" )

(define fvwm2-module-send-window-list ((@ app scwm fvwm-module) 'fvwm2-module-send-window-list))
(define fvwm2-module-send-packet ((@ app scwm fvwm-module) 'fvwm2-module-send-packet))
(define longs->string ((@ app scwm fvwm-module) 'longs->string))

(fvwm2-module-send-window-list (current-output-port))

(uniform-array-write (make-uniform-array (array-prototype "str")) (current-output-port))

(make-uniform-array (array-prototype "str"))

(binary-write "foo")

M_NEW_PAGE
