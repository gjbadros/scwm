;;;; $Id$

(use-scwm-modules test-case)

;; move-group-relative has required and optional arguments
(test-case 
 ""
 (procedure-required-formals move-group-relative)
 => '(dx dy))

(test-case
 ""
 (procedure-keyword-formals move-group-relative)
 => '())

(test-case
 ""
 (procedure-optional-formals move-group-relative)
 => '(group))

;; round/ has only required arguments
(test-case
 ""
 (procedure-required-formals round/)
 => '(x y))

(test-case
 ""
 (procedure-keyword-formals round/)
 => #f)

(test-case
 ""
 (procedure-optional-formals round/)
 => #f)

;; set-window-foreground! is a C primitive w/ required and optional arguments
(test-case 
 ""
 (procedure-formals set-window-foreground!)
 => '(fg win))

(test-case
 ""
 (procedure-required-formals set-window-foreground!)
 => '(fg))

(test-case
 ""
 (procedure-keyword-formals set-window-foreground!)
 => #f)

(test-case
 ""
 (procedure-optional-formals set-window-foreground!)
 => '(win))

;; flash-window has no required arguments, but has both optional and keyword args
(test-case
 ""
 (procedure-required-formals flash-window)
 => '())

(test-case
 ""
 (procedure-keyword-formals flash-window)
 => '((color "red") (unflash-delay 0.5) (continually #f)))

(test-case
 ""
 (procedure-optional-formals flash-window)
 => '(win))
