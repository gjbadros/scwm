(define win (get-window))

;; use 1 - 1000 for z values
(define default-z-value 500)
(define on-top-z-value 1)
(define on-bottom-z-value 1000)

(define (add-window-clv-z win)
  (let ((z (make-cl-variable  
	    (string-append (window-icon-title win) "/z")
	    default-z-value)))
    (set-window-property! win 'z z)
    (set-object-property! z 'window win)
    (cl-add-stay solver z)))

(define (clv->window var)
  (object-property var 'window))

;; (clv->window (window-clv-z (get-window)))

(define (window-clv-z win)
  (window-property win 'z))

(define (window-z-value win)
  (let ((clv-z (window-clv-z win)))
    (if clv-z (cl-value clv-z)
	(if (kept-on-top? win)
	    on-top-z-value
	    default-z-value))))

(add-window-clv-z (get-window))
(window-clv-z (get-window))
(window-z-value (get-window))

(define v (make-cl-variable))

(clv-attach! v 'foo)

(clv-attached-object v)

(define (keep-z-same w1 w2)
  (let ((w1-z (window-clv-z w1))
	(w2-z (window-clv-z w2)))
    (let ((cn (make-cl-constraint w1-z = w2-z)))
      (cl-add-constraint solver cn)
      cn)))

(keep-z-same (select-window-interactively "Pick w1")
	     (select-window-interactively "Pick w2"))

(cl-set-solver-var solver (window-clv-z (get-window)) 100)

;; create a list with the windows and their z values
(define (winlist-with-z-vals)
  (map (lambda (w) (cons w (window-z-value w))) (list-stacking-order)))

;; proc is like > -- takes two elements a & b
;; and returns #t iff a > b
;; (FIXGJB: not stable -- need a stable sort!)
(define (quicksort l proc)
  (if (< (length l) 2)
      l
      (let* ((first (car l))
	     (rest (cdr l))
	     (le-gt (partition first rest proc))
	     (le (car le-gt))
	     (gt (cadr le-gt)))
	(append (quicksort le proc) (list first) (quicksort gt proc)))))

(define (partition e l proc)
  (let ((le '())
	(gt '()))
    (for-each (lambda (v) (if (proc v e) 
			      (set! gt (cons v gt))
			      (set! le (cons v le)))) l)
    (list le gt)))

(partition 3 '(4 5 1 3 4 5 7) >)

(quicksort '(4 5 1 3 4 5 7) >)

(define (cars-only l)
  (map car l))

(define (winlist-sorted)
  (cars-only (quicksort (winlist-with-z-vals) 
			(lambda (a b) (> (cdr a) (cdr b))))))

(restack-windows (winlist-sorted))

(cl-set-solver-var solver (window-clv-z (get-window)) 600)
