(define xp (string->xproperty "GJBProp"))

(xproperty->string xp)

(xproperty->string (window-xproperty (current-window-with-focus) "WM_PROTOCOLS"))

(xproperty->string (window-xproperty (current-window-with-focus) "WM_CLIENT_MACHINE"))

(add-hook! X-PropertyNotify-hook (lambda (sz w) (write sz) (write w) (display "\n")))

(add-hook! shutdown-hook (lambda (sz) (display "hello") (write sz)))

(add-hook! before-new-window-hook (lambda (w) (write w) (display "\n")))


(set! X-PropertyNotify-hook #f)

(set-window-text-property (current-window-with-focus) "WM_FOO" "Testing GJB")

(xproperty->string (window-xproperty (current-window-with-focus) "WM_FOO"))

;;; these two lines cause a core dump --07/05/98 gjb
(add-hook! X-PropertyNotify-hook (lambda (sz w EXTRA-ARG) (write sz) (write w) (display "\n")))
(set-window-text-property (current-window-with-focus) "WM_FOO" "Testing GJB")
