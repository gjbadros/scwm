;; $Id$
;; (reset-scwm-exec-protocol)
(begin
  (define solver (make-cl-solver))
  (scwm-set-master-solver solver)
  ;(map (lambda (w) (add-stays-on-window w)) (list-all-windows))
  (define wA (select-window-interactively "Pick window A"))
  (define wB (select-window-interactively "Pick window B"))
  (define v (make-cl-variable "v"))
  (cl-add-stay solver v)

  (define (keep-tops-even w1 w2)
    (let ((w1-y (window-clv-y w1))
	  (w2-y (window-clv-y w2)))
      (let ((cn (make-cl-constraint w1-y = w2-y)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-lefts-even w1 w2)
    (let ((w1-x (window-clv-x w1))
	  (w2-x (window-clv-x w2)))
      (let ((cn (make-cl-constraint w1-x = w2-x)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-lefts-even w1 w2)
    (let ((w1-x (window-clv-x w1))
	  (w2-x (window-clv-x w2)))
      (let ((cn (make-cl-constraint w1-x = w2-x)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-bottoms-even w1 w2)
    (let ((w1-y (window-clv-y w1))
	  (w2-y (window-clv-y w2))
	  (w1-height (window-clv-height w1))
	  (w2-height (window-clv-height w2)))
      (let ((cn (make-cl-constraint
		 (cl-plus w1-y w1-height) =
		 (cl-plus w2-y w2-height))))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-rights-even w1 w2)
    (let ((w1-x (window-clv-x w1))
	  (w2-x (window-clv-x w2))
	  (w1-width (window-clv-width w1))
	  (w2-width (window-clv-width w2)))
      (let ((cn (make-cl-constraint
		 (cl-plus w1-x w1-width) =
		 (cl-plus w2-x w2-width))))
	(cl-add-constraint solver cn)
	cn)))
  
  (define (keep-to-left-of w1 w2)
    (let ((w1-x (window-clv-x w1))
	  (w1-width (window-clv-width w1))
	  (w2-x (window-clv-x w2)))
      (let ((cn (make-cl-constraint (cl-plus w1-width w1-x) <= w2-x)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-top-at-v w1)
    (let ((w1-y (window-clv-y w1)))
      (let ((cn (make-cl-constraint w1-y = v)))
	(cl-add-constraint solver cn)
	cn)))
  
  (define (keep-left-at-v w1)
    (let ((w1-x (window-clv-x w1)))
      (let ((cn (make-cl-constraint w1- x = v)))
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
    (let ((w1-right-x (cl-plus (window-clv-x w1) (window-clv-width w1)))
	  (w2-x (window-clv-x w2)))
      (let ((cn (make-cl-constraint w1-right-x = w2-x)))
	(cl-add-constraint solver cn)
	cn)))

  (define (keep-adjacent-vertical w1 w2)
    (let ((w1-bottom-y (cl-plus (window-clv-y w1) (window-clv-height w1)))
	  (w2-y (window-clv-y w2)))
      (let ((cn (make-cl-constraint w1-bottom-y = w2-y)))
	(cl-add-constraint solver cn)
	cn)))
  
  (define (keep-at-left-edge w)
    (let ((w-x (window-clv-x w)))
      (let ((cn (make-cl-constraint w-x = 0 cls-medium)))
	(cl-add-constraint solver cn)
	cn)))
  
  (define (keep-at-right-edge w)
    (let ((w-right (cl-plus (window-clv-x w) (window-clv-width w))))
      (let ((cn (make-cl-constraint w-right = 1152 cls-medium .5)))
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

(cl-constraint-list solver #f)

(define cn (car (cl-constraint-list solver)))

(cl-remove-constraint solver cn)

(define wA (select-window-interactively "Pick A"))
(define wB (select-window-interactively "Pick B"))
(define wC (select-window-interactively "Pick C"))

(keep-constant-width wA 200)
(keep-at-left-edge wA)
(keep-at-right-edge wA)

(keep-constant-width wB 200)
(keep-at-left-edge wB)
(keep-at-right-edge wB)


(window-position w)

(define cnl (keep-to-left-of wA wB))
(set-object-property! cnl 'description "Keep wA to left of wB")
(object-property (car (cl-constraint-list solver)) 'description)


(cl-remove-constraint solver cnl)

(keep-full-width wA wB)
(keep-adjacent-horizontal wA wB)
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


(for-each (lambda (x) (cl-set-solver-var solver v x) (usleep 100000))
	  '(10 20 30 40 50 60 70 80 90 100 110 120 130))

(keep-to-left-of (current-window-with-focus) wA)
(keep-to-left-of (current-window-with-focus) wB)

(window-clv-width (current-window-with-focus))

(window-clv-x (current-window-with-focus))

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


;;; Local Variables:
;;; eval: (progn (load "scwm") (scwm-mode))
;;; End:
