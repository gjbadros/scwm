;; $Id$
;; (scwm-path-prefix)
;; (documentation "resize-to")
;;

(resize-to 500 400 (select-window-interactively "Resize me"))

(define w (select-window-interactively))

(normal-border w)
(plain-border w)

(define doc-files        ; '("/usr/src/scwm/doc/scwm-procedures.txt")
  (list (string-append (scwm-path-prefix) "/share/scwm/scwm-procedures.txt")
	(string-append (scwm-path-prefix) "/share/scwm/cassowary-scm-procedures.txt")))

