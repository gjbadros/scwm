;;; $Id$
;;; send-string.scm
;;; Copyright (C) 1999, 2000 Greg J. Badros
;;;
;;; Permit sending full strings to applications using synthetic events
;;;


(define-module (app scwm send-string)
  :use-module (app scwm optargs))

(define-public (printable-char->keysym-string char)
  "Return the keysym string corresponding to a printable character.
CHAR is a scheme character.  The return value is appropriate for
use by `send-key'.  See also `X-synthetic-send-string'."
  (let ((charval (char->integer char))
	(char-keysym-alist
	 '((#\space . "space")
	   (#\newline . "Linefeed")
	   (#\cr . "Return")
	   (#\esc . "Escape")
	   (#\bs . "BackSpace")
	   (#\del . "Delete")
	   (#\tab . "Tab")
	   (#\! . "S-1") ;; "exclam"
	   (#\" . "S-apostrophe") ;; "quotedbl"
	   (#\# . "S-3") ;; "numbersign"
	   (#\$ . "S-4") ;; "dollar"
	   (#\% . "S-5") ;; "percent"
	   (#\& . "S-7") ;; "ampersand"
	   (#\' . "apostrophe")
	   (#\( . "S-9") ;; "parenleft"
	   (#\) . "S-0") ;; "parenright"
	   (#\* . "S-8") ;; "asterisk"
	   (#\+ . "S-equal") ;; "plus"
	   (#\, . "comma")
	   (#\- . "minus")
	   (#\. . "period")
	   (#\/ . "slash")
	   (#\: . "S-semicolon") ;; "colon"
	   (#\; . "semicolon")
	   (#\< . "less")
	   (#\= . "equal")
	   (#\> . "S-period") ;; "greater"
	   (#\? . "S-slash") ;; "question"
	   (#\@ . "S-2") ;; "at"
	   (#\[ . "bracketleft")
	   (#\\ . "backslash")
	   (#\] . "bracketright")
	   (#\^ . "S-6") ;; "caret"
	   (#\_ . "S-minus") ;; "underscore"
	   (#\` . "grave")
	   (#\' . "quoteleft")
	   (#\{ . "S-bracketleft") ;; "braceleft"
	   (#\| . "S-backslash") ;; "bar"
	   (#\} . "S-bracketright") ;; "brackeright"
	   (#\~ . "S-grave") ;; "asciitilde"
	   )))
    (let ((cell (assq char char-keysym-alist)))
      (cond
       (cell
	(cdr cell))
       ((< charval 32) (string-append "C-"
				      (make-string 1 (integer->char
						      (+ 64 charval)))))
       (#t (make-string 1 char))))))
;; (printable-char->keysym-string "")
;; (X-synthetic-send-string "!@#$%^&*()_+[]\\{}|;':\",./<>?`~" (get-window))


(define*-public (X-synthetic-send-string str #&optional (win (get-window)))
  "Send string STR to WIN via synthetic X events.
See also `send-key'.
Note that some programs (e.g., xterm) by default do not
honour synthetic key events as they are a security hole."
  (let ((i 0))
    (while (< i (string-length str))
	   (send-key
	    (printable-char->keysym-string (string-ref str i)) win)
	   (set! i (+ 1 i)))))
