;; $Id$

;; see if interactive move/size window reconfigures itself after this
(set-menu-font! (make-font "*helvetica*medium-r*12*"))

(set-menu-font! (make-font "*helvetica*medium-r*18*"))

(set-menu-font! (make-font "*helvetica*medium-r*24*"))


;;; weird interactions with this?
(menu-style #:fg "black" #:bg "gray80" #:stipple "grey35" 
	    #:font menu-font #:mwm #f)


;;; Local Variables:
;;; eval: (progn (load "scwm") (scwm-mode))
;;; End:
