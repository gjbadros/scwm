(use-modules (app scwm sort))

(define win (get-window))

;; (set-window-property! win 'z 4)
;; (window-property win 'z)
;; (set-object-property! win 'z 4)
;; (object-property win 'z)
;; (window? win)

(begin
  ;; use 1 - 1000 for z values
  (define default-z-value 500)
  (define on-top-z-value 1)
  (define on-bottom-z-value 1000)
  
  (define (add-window-clv-z win)
    (let ((z (make-cl-variable  
	      (string-append (window-icon-title win) "/z")
	      default-z-value)))
      (set-window-property! win 'z z)
      (set-object-property! z
			    'changed-proc
			    (lambda () (set! needs-restacking #t)))
      (set-object-property! z 'window win)
      (cl-add-stay solver z)
      z))
  
  (define (clv->window var)
    (object-property var 'window))
  
  
  (define (window-clv-z win)
    (window-property win 'z))
  
  (define (window-z-value win)
    (let ((clv-z (window-clv-z win)))
      (if clv-z (cl-value clv-z)
	  (if (kept-on-top? win)
	      on-top-z-value
	      default-z-value))))
  
  (define (keep-z-same w1 w2)
    (let ((w1-z (window-clv-z w1))
	  (w2-z (window-clv-z w2)))
      (let ((cn (make-cl-constraint w1-z = w2-z)))
	(cl-add-constraint solver cn)
	cn)))

  (define needs-restacking #f)
  )

(add-window-clv-z (get-window))
;; (window-clv-z (get-window))
;; (window-z-value (get-window))
;; (clv->window (window-clv-z (get-window)))

(define v (make-cl-variable))
(cl-add-constraint 
 solver (make-cl-constraint v = (window-clv-xl (get-window))))

;; (cl-remove-constraint solver (car (cl-constraint-list solver)))

;; (set-object-property! v 'changed-proc (lambda () (display "howdy\n")))
;; (object-property v 'changed-proc)
;; (cl-set-solver-var solver v 100)
;; (clv-attach! v 'foo)
;; (clv-attached-object v)

(begin
  ;; create a list with the windows and their z values
  (define (winlist-with-z-vals)
    (map (lambda (w) (cons w (window-z-value w))) (list-stacking-order)))
  
  (define (cars-only l)
    (map car l))

  (define (winlist-sorted)
    (cars-only (sort (winlist-with-z-vals) 
		     (lambda (a b) (> (cdr a) (cdr b))))))
  
  (define (restack-windows-for-z-value)
    (restack-windows (winlist-sorted)))

  (define (window-set-z-value win z-value)
    (let ((clv-z (window-clv-z win)))
      (if (not clv-z)
	  (set! clv-z (add-window-clv-z win)))
      (cl-set-solver-var solver clv-z z-value)))
  )

(keep-z-same (select-window-interactively "Pick w1")
	     (select-window-interactively "Pick w2"))

(add-hook! scwm-resolve-hook (lambda (solver)
			       (restack-windows-for-z-value)
			       (set! needs-restacking #f)))

;; (cl-set-solver-var solver (window-clv-z (get-window)) 100)
;; (cl-set-solver-var solver (window-clv-z (get-window)) 101)
;; (cl-set-solver-var solver (window-clv-z (get-window)) 600)
;; (cl-set-solver-var solver (window-clv-z (get-window)) 601)

(window-set-z-value (get-window) 400)
(window-set-z-value (get-window) 600)
(window-z-value (get-window))

;; FIXGJB: still need to make the existing raise/lower
;; procedures manipulate the z values instead of just
;; calling the X functions
