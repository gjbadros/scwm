; (load "test.scm")

;; atoms

(test-case "X-atom -> string"
 (X-atom->string 1)
 => "PRIMARY")

(test-case "string -> X-atom"
 (string->X-atom "SECONDARY")
 => 2)

(test-case "string -> X-atom -> string"
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

(test-case "Get a string property (WM_NAME)"
 (X-property-get xlogo "WM_NAME")
 => '("xlogo" "STRING" 8))

(test-case "Get a format 32 property (WM_PROTOCOLS)"
 (X-property-get xlogo "WM_PROTOCOLS")
 => `(,(list->vector (list (string->X-atom "WM_DELETE_WINDOW"))) "ATOM" 32))

(test-case "Get the icon window id from WM_STATE"
  (vector-ref (car (X-property-get (current-window-with-focus) "WM_STATE")) 1)
  #t)

; a list of strings
(test-case "Get a string property containing multiple strings (WM_COMMAND)"
 (separate-fields-discarding-char #\null
				  (car (X-property-get
					xlogo
					"WM_COMMAND"))
				  list)
 => '("xlogo" "-g" "123x123" ""))

(delete-window xlogo)

(test-case "Set my own text property"
 (X-property-set! 'root-window "WM_FOO" "Testing ... 1 ... 2")
 #t)

(test-case "Consume my own property"
 (X-property-get 'root-window "WM_FOO" #t)
 => '("Testing ... 1 ... 2" "STRING" 8))

(test-case "Get an already consumed property"
 (X-property-get 'root-window "WM_FOO")
 => #f)

(test-case "Set my own format 16 property"
 (X-property-set! 'root-window "WM_FOO" #(-2 3) "WM_BAR" 16)
 #t)

(test-case "Get my own format 16 property"
 (X-property-get 'root-window "WM_FOO")
 => '(#(-2 3) "WM_BAR" 16))

(test-case "Prepend to my own format 16 property"
 (X-property-set! 'root-window "WM_FOO" #(1) "WM_BAR" 16 'prepend)
 #t)

(test-case "Get the property with prependix"
 (X-property-get 'root-window "WM_FOO" #f)
 => '(#(1 -2 3) "WM_BAR" 16))

(test-case "Append to my own format 16 property"
 (X-property-set! 'root-window "WM_FOO" #(4 -5 6) "WM_BAR" 16 'append)
 #t)

(test-case "Get the property with prependix and appendix"
 (X-property-get 'root-window "WM_FOO" #t)
 => '(#(1 -2 3 4 -5 6) "WM_BAR" 16))

; error conditions
(test-case "Set a property with an illegal mode"
 (X-property-set! 'root-window "WM_FOO" #(1) "WM_BAR" 16 'root-window)
 #f)

(test-case "Set a property with value out of format 8 range"
 (X-property-set! 'root-window "WM_FOO" #(500 0) "WM_BAR" 8)
 #f)

(test-case "Set a property with value out of format 16 range"
 (X-property-set! 'root-window "WM_FOO" #(-100000 0) "WM_BAR" 16)
 #f)

;; hooks

(add-hook! X-PropertyNotify-hook (lambda (sz w) (write sz) (write w) (display "\n")))

(add-hook! shutdown-hook (lambda (sz) (display "hello") (write sz)))

(add-hook! before-new-window-hook (lambda (w) (write w) (display "\n")))


(set! X-PropertyNotify-hook #f)


;;; causes a core dump --07/05/98 gjb
(add-hook! X-PropertyNotify-hook (lambda (sz w EXTRA-ARG) (write sz) (write w) (display "\n")))
