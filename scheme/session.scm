;;; $Id$
;;; session.scm
;;; Copyright (C) 1999, 2000 Greg J. Badros
;;;

(define-module (app scwm sesssion)
  :use-module (ice-9 session)
  :use-module (ice-9 regex)
  )

;; (string->scwm-module "app scwm base")
;; (string->scwm-module "(app scwm base)")
;; (string->scwm-module "base")

;;(define module-name "app scwm base")
;;(define module-name "(app scwm base)")
;;(define module-name "base")
(define-public (string->scwm-module module-name)
  "Return the module corresponding to the given string.
Format may be any of \"base\", \"app scwm base\", or
\"(app scwm base)\"."
  (let ((modref
	 (cond 
	  ((string-match "^\\((.*)\\)" module-name)
	   => (lambda (m)
		(split-c-module-name (match:substring m 1))))
	  ((string-match " " module-name)
	   => (lambda (m)
		(split-c-module-name module-name)))
	  (else (list 'app 'scwm (string->symbol module-name))))))
    (resolve-module modref)))
