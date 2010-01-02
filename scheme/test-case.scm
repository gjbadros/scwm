;; $Id$

(define-module (app scwm test-case))

;;; Controls whether test case failures are displayed or not.
(define-public display-test-case-errors #f)

;;; Test FORM for proper results.
;;; TITLE is just for documentation and ignored at the moment.
;;; (test-case TITLE FORM #f) returns #t, if FORM throws an error; #f otherwise.
;;; (test-case TITLE FORM #t) returns #t, if FORM eval's successfully;
;;;				    #f otherwise.
;;; (test-case TITLE FORM => RES) returns #t, if FORM eval's to RES;
;;;				    #f otherwise.
(defmacro-public test-case (TITLE FORM . RESULT)
  (catch #t
	 (lambda ()
	   (let ((res (eval FORM (current-module)))
		 (succeed (car RESULT)))
	     (cond
	      ((eq? succeed '=>) 
	       (let ((answer (equal? res (eval (cadr RESULT) (current-module)))))
		 (if (and display-test-case-errors (not answer) )
		     (begin
		       (display "Test `")
		       (display (symbol->string TITLE))
		       (display "' failed -- `")
		       (display res)
		       (display "' not equal? to `")
		       (display (eval (cadr RESULT) (current-module)))
		       (display "' in ")
		       (display FORM)
		       (display "\n")))
		 answer))
	      ((eq? succeed #t) #t)
	      (else #f))))
	 (lambda (key . args)
	   (eq? (car RESULT) #f))))
