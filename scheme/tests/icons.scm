;;; $Id$ -*- scwm -*-

(window-style "*" 
	      #:fg "black" #:bg "grey76" 
	      #:icon #t
	      #:show-icon #t
	      #:icon-box (list (x- 70) 1 69 (y- 141))
	      #:border-width 4 ;; MS borderwidth of 4 looks slightly better
	                       ;; to my eye than 3.
	      #:focus 'mouse

;	      #:plain-border #t ;; replaces handle-width
				;; MS no, it replaces no-handles, plus
	                        ;; it doesn't work right now.
	      #:sticky-icon #t
	      #:random-placement #t #:smart-placement #t
	      #:mwm-func-hint #t #:mwm-decor-hint #t
	      #:int-override #t #:decorate-transient #f
	      #:PPosition-hint #f
	      #:mini-icon pic-xterm-mini)

(icon-position (select-window-interactively))
