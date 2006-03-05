;;; $Id$
;;; Copyright (C) 1999, 2000 Greg J. Badros

(define-module (app scwm simple-constraints)
  :use-module (cassowary constraints)
  :use-module (app scwm optargs))

(define*-public (keep-tops-even w1 w2 #:optional (enable? #f))
  "Keep W1 and W2 aligned at the top."
  (let ((w1-y (window-clv-yt w1))
        (w2-y (window-clv-yt w2)))
    (let ((cn (make-cl-constraint w1-y = w2-y)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-lefts-even w1 w2 #:optional (enable? #f))
  "Keep W1 and W2 aligned at the left."
  (let ((w1-x (window-clv-xl w1))
        (w2-x (window-clv-xl w2)))
    (let ((cn (make-cl-constraint w1-x = w2-x)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-bottoms-even w1 w2 #:optional (enable? #f))
  "Keep W1 and W2 aligned at the bottom."
  (let ((w1-y (window-clv-yb w1))
        (w2-y (window-clv-yb w2)))
    (let ((cn (make-cl-constraint w1-y = w2-y)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-rights-even w1 w2 #:optional (enable? #f))
  "Keep W1 and W2 aligned at the right."
  (let ((w1-x (window-clv-xr w1))
        (w2-x (window-clv-xr w2)))
    (let ((cn (make-cl-constraint w1-x = w2-x)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-to-left-of w1 w2 #:optional (enable? #f))
  "Keep W1 to the left of W2."
  (let ((w1-xr (window-clv-xr w1))
        (w2-xl (window-clv-xl w2)))
    (let ((cn (make-cl-constraint w1-xr <= w2-xl)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-above w1 w2 #:optional (enable? #f))
  "Keep W1 wholly above W2."
  (let ((w1-yb (window-clv-yb w1))
        (w2-yt (window-clv-yt w2)))
    (let ((cn (make-cl-constraint w1-yb <= w2-yt)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-top-at-v w1 v #:optional (enable? #f))
  "Keep W1's top at cl-variable V's pixel value.
Use `cl-set-solver-var' to change V."
  (let ((w1-y (window-clv-yt w1)))
    (let ((cn (make-cl-constraint w1-y = v)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-left-at-v w1 v #:optional (enable? #f))
  "Keep W1's left at cl-variable V's pixel value.
Use `cl-set-solver-var' to change V."
  (let ((w1-x (window-clv-xl w1)))
    (let ((cn (make-cl-constraint w1-x = v)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-right-at-v w1 v #:optional (enable? #f))
  "Keep W1's right at cl-variable Vs pixel value.
Use `cl-set-solver-var' to change V."
  (let ((w1-x (window-clv-xr w1)))
    (let ((cn (make-cl-constraint w1-x = v)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-full-width w1 w2 #:optional (enable? #f))
  "Keep W1 and W2 sum of widths equal to the display's width."
  (let ((w1-width (window-clv-width w1))
        (w2-width (window-clv-width w2)))
    (let ((cn (make-cl-constraint (cl-plus w1-width w2-width) = 1152)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-full-height w1 w2 #:optional (enable? #f))
  "Keep W1 and W2 sum of heights equal to the display's height."
  (let ((w1-height (window-clv-height w1))
        (w2-height (window-clv-height w2)))
    (let ((cn (make-cl-constraint (cl-plus w1-height w2-height) = 900)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-adjacent-horizontal w1 w2 #:optional (enable? #f))
  "Keep W1's right edge aligned with W2's left edge."
  (let ((w1-xr (window-clv-xr w1))
        (w2-xl (window-clv-xl w2)))
    (let ((cn (make-cl-constraint w1-xr = w2-xl)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-adjacent-vertical w1 w2 #:optional (enable? #f))
  "Keep W1's bottom edge aligned with W2's top edge."
  (let ((w1-yb (window-clv-yb w1))
        (w2-yt (window-clv-yt w2)))
    (let ((cn (make-cl-constraint w1-yb = w2-yt)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-at-left-edge w #:optional (enable? #f))
  "Keep W's left edge at the left edge of the desktop."
  (let ((w-xl (window-clv-xl w)))
    (let ((cn (make-cl-constraint w-xl = 0 cls-strong .1)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-at-top-edge w #:optional (enable? #f))
  "Keep W's top edge at the top edge of the desktop."
  (let ((w-yt (window-clv-yt w)))
    (let ((cn (make-cl-constraint w-yt = 0)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-at-right-edge w #:optional (enable? #f))
  "Keep W's right edge at the right edge of the desktop."
  (let ((w-xr (window-clv-xr w)))
    (let ((cn (make-cl-constraint w-xr = 1152 cls-strong .1)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

(define*-public (keep-constant-width w width #:optional (enable? #f))
  "Keep W's width at WIDTH."
  (let ((w-width (window-clv-width w)))
    (let ((cn (make-cl-constraint w-width = width cls-strong .5)))
      (and enable? (cl-add-constraint (scwm-master-solver) cn))
      cn)))

;; returns the stay constraints, perhaps to be attached to 
;; the window object
(define*-public (keep-constant-size w #:optional (enable? #f))
  "Keep W's size from changing."
  (let ((cnw (make-cl-stay-constraint (window-clv-width w) cls-strong 2))
        (cnh (make-cl-stay-constraint (window-clv-height w) cls-strong 2)))
    (and enable? (cl-add-constraint (scwm-master-solver) cnw cnh))
    (list cnw cnh)))

(define-public (cl-set-solver-var s clv value)
  "Set solver S's variable CLV to value VALUE."
  (cl-add-editvar s clv)
  (cl-begin-edit s)
  (cl-suggest-value s clv value)
  (cl-end-edit s))

;;; Local Variables:
;;; End:
