;;; $Id$
;;; basic-styles.scm
;;; (C) 1999 Greg J. Badros
;;;

(define-module (app scwm basic-styles)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm style))

(define-public desk-icon
  (make-style #:winlist-skip #t
	      #:circulate-skip #t #:focus 'none))

(define-public desk-widget
  (make-style #:plain-border #t #:sticky #t #:winlist-skip #t
	      #:border-width 3 #:circulate-skip #t #:focus 'none))

(define-public desk-widget-on-top
  (make-style #:plain-border #t #:sticky #t #:winlist-skip #t
	      #:border-width 3 #:circulate-skip #t #:focus 'none
	      #:kept-on-top #t))

(define-public desk-widget-on-top-no-titlebar
  (make-style #:plain-border #t #:sticky #t #:winlist-skip #t
	      #:border-width 3 #:circulate-skip #t #:focus 'none
	      #:kept-on-top #t #:no-titlebar #t))

(define-public desk-widget-no-titlebar
  (make-style #:plain-border #t #:sticky #t #:winlist-skip #t
	      #:border-width 3 #:circulate-skip #t #:focus 'none
	      #:no-titlebar #t))
