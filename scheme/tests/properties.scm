; (load "test.scm")

;; atoms

(test-case
 (X-atom->string 1)
 => "PRIMARY")

(test-case
 (string->X-atom "SECONDARY")
 => 2)

(test-case
  (X-atom->string (string->X-atom "WM_FOO"))
  => "WM_FOO")


;; properties
(execute "xlogo -g 123x123")

(define xlogo (car (list-windows
		    #:only (lambda (w)
			     (let ((size (window-size w)))
			       (and (eqv? (car size) 123)
				    (eqv? (cadr size) 123)
				    (wildcard-match? "xlogo" w)))))))

; a string property
(test-case
 (X-property-get xlogo "WM_NAME")
 => '("xlogo" "STRING" 8))

; a format-32 property
(test-case
 (X-property-get xlogo "WM_PROTOCOLS")
 => `(,(list->vector (list (string->X-atom "WM_DELETE_WINDOW"))) "ATOM" 32))

; likewise, get the icon window id
(vector-ref (car (X-property-get (current-window-with-focus) "WM_STATE")) 1)

; a list of strings
(test-case
 (separate-fields-discarding-char #\null
				  (car (X-property-get
					xlogo
					"WM_COMMAND"))
				  list)
 => '("xlogo" "-g" "123x123" ""))

(delete-window xlogo)

; set a property
(test-case
 (X-property-set! 'root-window "WM_FOO" "Testing ... 1 ... 2")
 #t)

; consume it
(test-case
 (X-property-get 'root-window "WM_FOO" #t)
 => '("Testing ... 1 ... 2" "STRING" 8))

(test-case
 (X-property-get 'root-window "WM_FOO")
 => #f)

(test-case
 (X-property-set! 'root-window "WM_FOO" #(-2 3) "WM_FOO" 16)
 #t)

(test-case
 (X-property-get 'root-window "WM_FOO")
 => '(#(-2 3) "WM_FOO" 16))

(test-case
 (X-property-set! 'root-window "WM_FOO" #(1) "WM_FOO" 16 'prepend)
 #t)

(test-case
 (X-property-get 'root-window "WM_FOO" #f)
 => '(#(1 -2 3) "WM_FOO" 16))

(test-case
 (X-property-set! 'root-window "WM_FOO" #(4 -5 6) "WM_FOO" 16 'append)
 #t)

(test-case
 (X-property-get 'root-window "WM_FOO" #t)
 => '(#(1 -2 3 4 -5 6) "WM_FOO" 16))

; error conditions
(test-case
 (X-property-set! 'root-window "WM_FOO" #(1) "WM_FOO" 16 'root-window)
 #f)

(test-case
 (X-property-set! 'root-window "WM_FOO" #(500 0) "WM_FOO" 8)
 #f)

(test-case
 (X-property-set! 'root-window "WM_FOO" #(-100000 0) "WM_FOO" 16)
 #f)

;; hooks

(add-hook! X-PropertyNotify-hook (lambda (sz w) (write sz) (write w) (display "\n")))

(add-hook! shutdown-hook (lambda (sz) (display "hello") (write sz)))

(add-hook! before-new-window-hook (lambda (w) (write w) (display "\n")))


(set! X-PropertyNotify-hook #f)


;;; causes a core dump --07/05/98 gjb
(add-hook! X-PropertyNotify-hook (lambda (sz w EXTRA-ARG) (write sz) (write w) (display "\n")))
