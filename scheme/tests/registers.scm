;; $Id$

(get-register-alist)

(register-value-type (get-register 't))
(register-value-type (get-register 'p))
(register-value-type (get-register 'o))
(register-value-type (get-register 'l))
(register-value-type (get-register 'g))

(define-public (register-type-mapping)
  (map (lambda (cell) 
	 (string-append (symbol->string (car cell))
			" -> "
			(symbol->string (register-value-type (cdr cell)))))
       (sort (get-register-alist) (lambda (cell) (string<? (symbol->string (car cell)))))))
