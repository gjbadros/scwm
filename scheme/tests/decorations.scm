;; $Id$
;; (scwm-path-prefix)
;; (documentation "resize-to")
;;

(resize-to 500 400 (select-window-interactively "Resize me"))

(define w (select-window-interactively))

(normal-border w)
(plain-border w)
