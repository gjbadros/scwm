; Test FORM for proper results.
; TITLE is just for documentation and ignored at the moment.
; (test-case TITLE FORM #f) returns #t, if FORM throws an error; #f otherwise.
; (test-case TITLE FORM #t) returns #t, if FORM eval's successfully;
;				    #f otherwise.
; (test-case TITLE FORM => RES) returns #t, if FORM eval's to RES;
;				    #f otherwise.
(defmacro-public test-case (TITLE FORM . RESULT)
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
