;;; $Id$
;;; stringops.scm
;;; Copyright (C) 1999 Greg J. Badros
;;;
;;; Various useful string operations.

(define-module (app scwm stringops)
  :use-module (ice-9 regex)
  :use-module (app scwm base)
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
  "Returns a string representation of the numerical WIN-STATE."
  (if (array-in-bounds? WindowStates win-state)
      (array-ref WindowStates win-state)
      "Invalid"))

(define Gravities #("Forget" "NorthWest" "North" "NorthEast" "West" "Center"
			     "East" "SouthWest" "South" "SouthEast" "Static"))

(define-public (gravity->string gravity)
  "Returns a string representation of the numerical GRAVITY."
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
  (let ((result ""))
    (map (lambda (el)
	   (if (not (string-null? el))
	       (if (string-null? result)
		   (set! result el)
		   (set! result (string-append result delimit el)))))
	 strings)
    result))

;; contributed by Glenn Trig
;; (this is close to basename, but keeps the path intact,
;;  whereas basename strips the leading directories, too --03/27/99 gjb)
(define-public (remove-suffix str suffix) 
  "Return STR with trailing SUFFIX removed if it exists."
  (let ((sufl (string-length suffix)) 
        (sl (string-length str))) 
    (if (and (> sl sufl) 
             (string=? (substring str (- sl sufl) sl) suffix)) 
        (substring str 0 (- sl sufl)) str)))

(define (string-has-prefix-helper string prefix proc)
  (let ((pref-length (string-length prefix)))
    (if (> pref-length (string-length string))
	#f
	(let ((str-pref (substring string 0 (string-length prefix))))
	  (proc str-pref prefix)))))

(define-public (string-has-prefix string prefix)
  "Return #t iff STRING starts with PREFIX."
  (string-has-prefix-helper string prefix string=?))

(define-public (string-ci-has-prefix string prefix)
  "Return #t iff STRING starts with PREFIX ignoring case."
  (string-has-prefix-helper string prefix string-ci=?))


;; (string-has-prefix "foo" "f")
;; (string-has-prefix "foo" "fo")
;; (string-has-prefix "foo" "foo")
;; (string-has-prefix "foo" "fooo")
;; (string-has-prefix "foo" "Fo")
;; (string-has-prefix "foo" "b")
