(define unknown-i (make-image "NoName.xpm"))
(define std-style
  (make-style #:fg "black" #:bg "grey76" #:sticky-icon #t
	      #:icon unknown-i
	      #:icon-box (list 500 1 100 200) ; (x- 70) 1 69 (y- 141))
	      #:border-width 3 #:mwm-border #t #:focus 'sloppy ; mouse
	      #:random-placement #t #:smart-placement #t
	      #:mwm-func-hint #t #:mwm-decor-hint #t #:int-override #t
	      #:decorate-transient #t #:PPosition-hint #f
	      #:mini-icon term-i #:use-decor std-d))

;; NOTE: you can use a string or a (make-image "filename.xpm")
;; after #:icon and #:mini-icon
(window-style "*" #:use-style std-style)
