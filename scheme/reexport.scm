
;; From thi's THUD --10/20/99 gjb

;; Re-export variables from module named OTHER-MODULE-NAME.  If SPECIFICALLY
;; is the empty list, all public variables from OTHER-MODULE-NAME are
;; exported, otherwise, each element in SPECIFICALLY is taken to be either a
;; symbol naming a variable to be exported, or a pair of symbols of the form
;; (OLD-NAME . NEW-NAME) describing the mapping to be used.  OLD-NAME should
;; name an exported variable in OTHER-MODULE-NAME.
;;
(defmacro reexport-from-module (other-module-name . specifically)
  `(let ((cur-mod   (current-module))
	 (other-mod (resolve-module ',other-module-name))
	 (spec      (map (lambda (x) (if (pair? x) x (cons x x))) ; for assq
			 ',specifically)))
     (let ((add! (lambda (old-name new-name)
		   (module-add! (module-public-interface cur-mod)
				new-name
				(module-variable other-mod old-name)))))
       (module-map (lambda (sym x)
		     (if (eq? '() spec)
			 (add! sym sym)
			 (let ((try (assq sym spec)))
			   (and try (add! (car try) (cdr try))))))
		   (module-public-interface other-mod)))))
