; Test FORM for proper results.
; (test-case FORM #f) returns #t, if FORM throws an error; #f otherwise.
; (test-case FORM #t) returns #t, if FORM eval's successfully; #f otherwise.
; (test-case FORM => RES) returns #t, if FORM eval's to RES; #f otherwise.
(defmacro-public test-case (FORM . RESULT)
  (catch #t
	 (lambda ()
	   (let ((res (eval FORM))
		 (succeed (car RESULT)))
	     (cond
	      ((eq? succeed '=>) (equal? res (eval (cadr RESULT))))
	      ((eq? succeed #t) #t)
	      (else #f))))
	 (lambda (key . args)
	   (eq? (car RESULT) #f))))
