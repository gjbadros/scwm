;;; $Id$
;;; flux.scm
;;; Copyright (C) 1999 Greg J. Badros
;;;
;;; Various useful string operations.

(define-module (app scwm stringops)
  :use-module (ice-9 regex)
  :use-module (app scwm optargs))

(define-public (make-string-usable-for-resource-key string)
  "Return a converted string from STRING that can be used as an X resource key.
The returned string will have all non-alphanumeric characters replaced with
underscores, so that the resulting string can be used as a key for
`X-resource-get' and `X-resource-put'."
  (regexp-substitute/global
   #f "[^a-zA-Z_0-9]" string
   'pre (lambda (match) "_")
   'post))
;;; (make-string-usable-for-resource-key "foo bar baz")
;;; (make-string-usable-for-resource-key "foo*bar.baz")


(define-public (bool->string arg)
  "Return the string \"false\" if ARG is #f, \"true\" otherwise."
  (if arg "true" "false"))

(define-public (color->string color)
  "Convert scwm color object COLOR into an X11 name of that color.
The resulting string can, e.g., be used in command lines for executing
other applications."
  (color-property color 'name))

(define*-public (size->string sz #&optional (sep "x"))
  "Convert a two-element list to a string.
Use the optional second argument as the separator."
  (string-append (number->string (car sz)) sep (number->string (cadr sz))))

(define-public (number->hex-string n)
  "A convenience wrapper around `number->string' that returns N in base-16."
  (number->string n 16))

(define WindowStates #("Withdrawn" "Normal" "Zoom" "Iconic" "Inactive"))

(define-public (window-state->string win-state)
  "Returns a string representation of the numerical WIN-STATE"
  (if (array-in-bounds? WindowStates win-state)
      (array-ref WindowStates win-state)
      "Invalid"))

(define Gravities #("Forget" "NorthWest" "North" "NorthEast" "West" "Center"
			     "East" "SouthWest" "South" "SouthEast" "Static"))

(define-public (gravity->string gravity)
  "Returns a string representation of the numerical GRAVITY"
  (if (array-in-bounds? Gravities gravity)
      (array-ref Gravities gravity)
      "Invalid"))

;; GJB:FIXME:: sans-final-newline exists in (ice-9 string-fun)
(define-public (chop-newline str)
  "Return STR up to but not including the first newline character."
  (let ((ich (string-index str #\newline)))
    (if (not ich)
	str
	(substring str 0 ich))))

(define-public (write-all port . lst)
  "Write all arguments into the port. #t means `current-output-port'."
  (if (eq? port #t) (set! port (current-output-port)))
  (do ((zz lst (cdr zz))) ((null? zz))
    (if (string? (car zz)) (display (car zz) port) (write (car zz) port))))

(define-public (to-string . rest)
  "Dump all arguments into a string."
  (with-output-to-string (lambda () (apply write-all #t rest))))

(define-public (string-join delimit strings)
  "Concatenates the list STRINGS into a single string.
DELIMIT is put between every two elements of STRINGS."
  (let ((result #f))
    (map (lambda (el)
	   (if (not (equal? el ""))
	       (if result
		   (set! result (string-append result delimit el))
		   (set! result el))))
	 strings)
    result))