;; not particularly efficient, but fast to hack together --09/05/98 gjb
;;
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

;;(partition 3 '(4 5 1 3 4 5 7) >)

;; (quicksort '(4 5 1 3 4 5 7) >)
;; (sort '(4 5 1 3 4 5 7) <)
  
