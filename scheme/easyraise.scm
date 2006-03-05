;;;; $Id$
;;;; easyraise.scm
;;;; Copyright (C) 2000 Greg J. Badros <gjb@cs.washington.edu>
;;;;

(define-module (app scwm easyraise)
  :use-module (app scwm optargs)
  :use-module (app scwm defoption)
  :use-module (app scwm style)
  :use-module (app scwm winlist)
  :use-module (app scwm listops)
  :use-module (app scwm style-options))

(define-public easyraise-windows #f)
(define-public easyraise-last-was-raise #f)

;; SRL:FIXME::The caching of easyraise windows in easyraise-windows can end up
;;   with invalid windows which never get removed from easyraise-windows.  This
;;   wastes memory with all of these invalid windows.

(define*-public (set-easyraise-window! easyraise? #:optional (win (get-window)))
  ""
  (if win 
      (begin
        (set-object-property! win 'easyraise easyraise?)
        (if easyraise-windows 
            (if easyraise?
                (if (not (memq win easyraise-windows))
                    (set! easyraise-windows (cons win easyraise-windows)))
                (set! easyraise-windows (delq! win easyraise-windows)))))))

(define*-public (easyraise-window? #:optional (win (get-window)))
  ""
  (object-property win 'easyraise))

;; (set-easyraise-window! #t (get-window))
(add-window-style-option #:easyraise set-easyraise-window!)

(define-public (list-easyraise-windows)
  (list-windows #:only easyraise-window?))

(define*-public (raise-easyraise-windows)
  ""
  (interactive)
  (if (not easyraise-windows)
      (set! easyraise-windows (list-easyraise-windows)))
  (for-each raise-window easyraise-windows)
  (set! easyraise-last-was-raise #t))

(define*-public (lower-easyraise-windows)
  ""
  (interactive)
  (if (not easyraise-windows)
      (set! easyraise-windows (list-easyraise-windows)))
  (for-each lower-window easyraise-windows)
  (set! easyraise-last-was-raise #f))

(define*-public (easyraise-window-is-hidden?)
  (or-map (lambda (win) (eq? (window-visibility win) 'fully-obscured)) (list-easyraise-windows)))

(define*-public (toggle-easyraise-windows)
  ""
  (interactive)
  (if (not easyraise-windows)
      (set! easyraise-windows (list-easyraise-windows)))
  (if (or 
       (not easyraise-last-was-raise)
       (easyraise-window-is-hidden?))
      (begin
	  (for-each raise-window easyraise-windows)
	  (set! easyraise-last-was-raise #t))
      (begin
	  (for-each lower-window easyraise-windows)
	  (set! easyraise-last-was-raise #f))))
