
; a string property
(X-property-get (current-window-with-focus) "WM_NAME")

; a format-32 property
(X-property-get (current-window-with-focus) "WM_PROTOCOLS")

; likewise, get the icon window id
(vector-ref (car (X-property-get (current-window-with-focus) "WM_STATE")) 1)

; a list of strings
(separate-fields-discarding-char #\null
				 (car (X-property-get
				       (current-window-with-focus)
				       "WM_COMMAND"))
				 list)

; set a property
(X-property-set! 'root-window "WM_FOO" "Testing ... 1 ... 2")

; consume it
(X-property-get 'root-window "WM_FOO" #t)

; should return #f
(X-property-get 'root-window "WM_FOO")

(X-property-set! 'root-window "WM_FOO" #(-2 3) "WM_FOO" 16)

(X-property-get 'root-window "WM_FOO")

(X-property-set! 'root-window "WM_FOO" #(1) "WM_FOO" 16 'prepend)

(X-property-get 'root-window "WM_FOO" #f)

(X-property-set! 'root-window "WM_FOO" #(4 -5 6) "WM_FOO" 16 'append)

(X-property-get 'root-window "WM_FOO" #t)

; errors should result from these:
(X-property-set! 'root-window "WM_FOO" #(1) "WM_FOO" 16 'root-window)

(X-property-set! 'root-window "WM_FOO" #(500 0) "WM_FOO" 8)

(X-property-set! 'root-window "WM_FOO" #(-100000 0) "WM_FOO" 16)

(X-property-set! 'root-window "WM_FOO" #(500 0) "WM_FOO" 8)

;; hooks

(add-hook! X-PropertyNotify-hook (lambda (sz w) (write sz) (write w) (display "\n")))

(add-hook! shutdown-hook (lambda (sz) (display "hello") (write sz)))

(add-hook! before-new-window-hook (lambda (w) (write w) (display "\n")))


(set! X-PropertyNotify-hook #f)


;;; causes a core dump --07/05/98 gjb
(add-hook! X-PropertyNotify-hook (lambda (sz w EXTRA-ARG) (write sz) (write w) (display "\n")))
