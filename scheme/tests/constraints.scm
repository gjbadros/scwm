;; $Id$
;; (reset-scwm-exec-protocol)
(begin
  (define swi select-window-interactively)
  (define solver (make-cl-solver))
  (scwm-set-master-solver solver)
  ;(map (lambda (w) (add-stays-on-window w)) (list-all-windows))
  (define v (make-cl-variable "v"))
  (cl-add-stay solver v)

  (define (keep-tops-even w1 w2)
    (let ((w1-y (window-clv-yt w1))
	  (w2-y (window-clv-yt w2)))
      (let ((cn (make-cl-constraint w1-y = w2-y)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-lefts-even w1 w2)
    (let ((w1-x (window-clv-xl w1))
	  (w2-x (window-clv-xl w2)))
      (let ((cn (make-cl-constraint w1-x = w2-x)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-bottoms-even w1 w2)
    (let ((w1-y (window-clv-yb w1))
	  (w2-y (window-clv-yb w2)))
      (let ((cn (make-cl-constraint w1-y = w2-y)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-rights-even w1 w2)
    (let ((w1-x (window-clv-xr w1))
	  (w2-x (window-clv-xr w2)))
      (let ((cn (make-cl-constraint w1-x = w2-x)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-to-left-of w1 w2)
    (let ((w1-xr (window-clv-xr w1))
	  (w2-xl (window-clv-xl w2)))
      (let ((cn (make-cl-constraint w1-xr <= w2-xl)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-above w1 w2)
    (let ((w1-yb (window-clv-yb w1))
	  (w2-yt (window-clv-yt w2)))
      (let ((cn (make-cl-constraint w1-yb <= w2-yt)))
	(cl-add-constraint solver cn)
	cn)))


  (define (keep-top-at-v w1)
    (let ((w1-y (window-clv-yt w1)))
      (let ((cn (make-cl-constraint w1-y = v)))
	(cl-add-constraint solver cn)
	cn)))
  
  (define (keep-left-at-v w1)
    (let ((w1-x (window-clv-xl w1)))
      (let ((cn (make-cl-constraint w1-x = v)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-right-at-v w1)
    (let ((w1-x (window-clv-xr w1)))
      (let ((cn (make-cl-constraint w1-x = v)))
	(cl-add-constraint solver cn)
	cn)))
  
  (define (keep-full-width w1 w2)
    (let ((w1-width (window-clv-width w1))
	  (w2-width (window-clv-width w2)))
      (let ((cn (make-cl-constraint (cl-plus w1-width w2-width) = 1152)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-full-height w1 w2)
    (let ((w1-height (window-clv-height w1))
	  (w2-height (window-clv-height w2)))
      (let ((cn (make-cl-constraint (cl-plus w1-height w2-height) = 900)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-adjacent-horizontal w1 w2)
    (let ((w1-xr (window-clv-xr w1))
	  (w2-xl (window-clv-xl w2)))
      (let ((cn (make-cl-constraint w1-xr = w2-xl)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-adjacent-vertical w1 w2)
    (let ((w1-yb (window-clv-yb w1))
	  (w2-yt (window-clv-yt w2)))
      (let ((cn (make-cl-constraint w1-yb = w2-yt)))
	(cl-add-constraint solver cn)
	cn)))
  
  (define (keep-at-left-edge w)
    (let ((w-xl (window-clv-xl w)))
      (let ((cn (make-cl-constraint w-xl = 0 cls-weak .1)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-at-right-edge w)
    (let ((w-xr (window-clv-xr w)))
      (let ((cn (make-cl-constraint w-xr = 1152 cls-weak .1)))
	(cl-add-constraint solver cn)
	cn)))
  
  (define (keep-constant-width w width)
    (let ((w-width (window-clv-width w)))
      (let ((cn (make-cl-constraint w-width = width cls-strong .5)))
	(cl-add-constraint solver cn)
	cn)))

  (define (cl-set-solver-var s clv value)
    (cl-add-editvar s clv)
    (cl-begin-edit s)
    (cl-suggest-value s clv value)
    (cl-end-edit s))
  )

;;(for-each (lambda (w) (write w) (write (window-position w))) (list-all-windows))
;;(for-each (lambda (w) (iconify w)) (list-all-windows))
;; (move-to 0 0 (id->window 29360129))

;; (window-position wA)
;; (window-position wB)

(begin
  (define wA (select-window-interactively "Pick window A"))
  (define wB (select-window-interactively "Pick window B"))
  (define wC (select-window-interactively "Pick C")))

(define cn (car (cl-constraint-list solver #f)))
(cl-remove-constraint solver cn)

(define cn-a-l-e (keep-at-left-edge wA))
(define cnl (keep-to-left-of wA wB))
(define cnlt (keep-above wA wB))
(define cnlt (keep-above wB wC))
(define cnah (keep-adjacent-horizontal wA wB))

(set-window-title! (swi) "XLOGO")
(show-titlebar (swi))
(hide-titlebar (swi))

(cl-constraint? cnl)
(cl-inequality? cnl)
(cl-equation? cnl)
(cl-constraint-list solver #f)
(cl-windows-of-constraint cnl)

;; Use blue for inequalities, red for equalities
(define (flash-windows-of-constraint cn)
  (let ((color (if (cl-inequality? cn) "blue" "red")))
    (for-each 
     (lambda (w) (flash-window w color)) 
     (cl-windows-of-constraint cn))))

(flash-windows-of-constraint cnl)
(flash-windows-of-constraint cnlt)
(flash-windows-of-constraint cnah)

(define cnl (keep-to-left-of (swi) (swi)))

(cl-is-constraint-satisfied? solver cnl)
(cl-is-constraint-satisfied? solver cn-a-l-e)
(keep-constant-width wA 200)
(keep-at-left-edge wA)
(keep-at-left-edge wB)
(keep-at-right-edge wB)
(define cnw (keep-constant-width wB 200))

(window-clv-width wB)
(window-clv-xl wB)

(cl-remove-constraint solver cnw)

(set-object-property! cnl 'description "Keep wA to left of wB")
(object-property (car (cl-constraint-list solver)) 'description)

;; remove all constraints
(for-each (lambda (cn) (cl-remove-constraint solver cn))
	  (cl-constraint-list solver))

(cl-remove-constraint solver cnl)

(keep-full-width wA wB)
(keep-adjacent-vertical wA wB)
(keep-adjacent-vertical (select-window-interactively) (select-window-interactively))
(keep-full-height wA wB)
(keep-full-height (select-window-interactively) (select-window-interactively))
(keep-to-left-of (current-window-with-focus) wA)
(keep-bottoms-even wA wB)
(keep-to-left-of wB wC)

(keep-lefts-even wA wB)
(keep-rights-even wA wB)
(keep-lefts-even (select-window-interactively) (select-window-interactively))
(keep-rights-even (select-window-interactively) (select-window-interactively))
(keep-tops-even wA wB)

(keep-full-height (get-window) (get-window))

(cl-solver-debug-print solver)

(keep-tops-even (current-window-with-focus) wB)
(keep-to-left-of wA wB)
(keep-top-at-v (current-window-with-focus))
(keep-left-at-v (current-window-with-focus))
(keep-left-at-v (select-window-interactively))

(cl-value v)
(cl-set-solver-var solver v 20)

(cl-value (window-clv-height wA))


(for-each (lambda (x) (cl-set-solver-var solver v x) (usleep 1000000))
	  '(10 20 30 40 50 60 70 80 90 100 110 120 130))

(keep-to-left-of (current-window-with-focus) wA)
(keep-to-left-of (current-window-with-focus) wB)

(window-clv-width (current-window-with-focus))

(window-clv-xl (current-window-with-focus))
(window-clv-xr (current-window-with-focus))
(window-clv-yt (current-window-with-focus))
(window-clv-yb (current-window-with-focus))

(add-stays-on-window (get-window))
(keep-to-left-of (get-window) (get-window))

(begin
  (define v1 (make-cl-variable))
  (define v2 (make-cl-variable))
  (define clsw1 (make-cl-weight 1 0 0))
  (define cls1 (make-cl-strength "cls1" clsw1))
  (define cls2 (make-cl-strength-3 "cls2" 2 1 0))
  (define e0 (make-cl-expression v1))
  (define e1 (cl-plus v1 v2))
  (define e2 (cl-plus e1 v1))
  (define e3 (cl-plus e1 4))
  (define e4 (cl-times e3 2))
  (define e5 (cl-minus e4 (cl-times 3 e1)))
  (define eq0 (make-cl-equation e0 cls1 2))
  (define eq1 (make-cl-equation e4))
  (define eq2 (make-cl-equation e5))
  (define eq3 (make-cl-equation (make-cl-expression v2)))
  (define ineq0 (make-cl-inequality e0 >= 1))
  (define ineq1 (make-cl-inequality e3 <= 1))
  (define solver (make-cl-solver))
  (define cn0 (make-cl-constraint e0 = e2))
  (cl-solver-debug-print solver))

(cl-inequality? cn0)
(cl-equation? cn0)
(cl-constraint? cn0)

cls-weak
cls-strong
cls-required

(define e4 (cl-times e2 v1))

(cl-add-constraint solver eq0)
(cl-add-constraint solver eq1)
(cl-add-constraint solver eq3)
(cl-solver-debug-print solver)
(cl-add-editvar solver v1)

(cl-variable? c)
(cl-variable? w1)
(cl-expression? e0)
(cl-expression? w1)
(cl-equation? eq0)
(cl-equation? w1)
(cl-inequality? ineq0)
(cl-inequality? w1)
(cl-solver? solver)
(cl-solver? w1)

;; virtual screen stuff -- very alpha, and needs a rewrite
(cl-set-solver-var solver (screen-clv-vx) 10)
(screen-clv-vx)
(screen-clv-vy)

;; kill all xlogo windows
(for-each (lambda (w) (close-window w)) 
	  (list-windows #:only (lambda (w) (string=? (window-title w) "xlogo"))))

;;(load "/scratch/gjb/scwm/scheme/flux.scm")
;;; Local Variables:
;;; End:
