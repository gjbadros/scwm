;;; $Id$
;;; doc.scm
;;; (C) 1998 Sam Steingold, Greg J. Badros, and Maciej Stachowiak




;; FIXGJB: hack to put doc-files in root module
(define-public doc-files        ; '("/usr/src/scwm/doc/scwm-procedures.txt")
  (map (lambda (s) (string-append (scwm-path-prefix) "/share/scwm/" s))
       '("scwm-procedures.txt" "cassowary_scm-procedures.txt")))

(define-module (app scwm doc)
  :use-module (ice-9 regex)
  :use-module (app scwm base)
  :use-module (app scwm flux)
  :use-module (app scwm optargs))



(define*-public (documentation func #&optional (port (current-output-port)))
  "Print the documentation for the string or symbol.
Return #t if found anything, #f if no documentation."
  (let* ((head (string-append
                "(" (if (string? func) func (symbol->string func))))
         (len (string-length head))
         (delim (lambda (st) (and (= 1 (string-length st))
                                  (char=? (string-ref st 0)  #\np)))))
    (do ((fl doc-files (cdr fl)) (done #f) (fd #f))
        ((or (null? fl) done)
         (if (not done) (write-all #t "No documentation for `" func "'\n"))
         done)
      (write-all #t "trying `" (car fl) "'...")
      (cond ((file-exists? (car fl)) (display "file exists\n")
             (set! fd (open-input-file (car fl)))
             (do ((ln (read-line fd) (read-line fd)) (delim-p #f))
                 ((or (eof-object? ln) done) (close-input-port fd))
               (cond ((delim ln) (set! delim-p #t))
                     ((and delim-p (< len (string-length ln))
                           (string=? head (substring ln 0 len))
                           (string-index " )" (string-ref ln len)))
                      (set! done #t)
                      (display ln port) (newline port)
                      (do ((ln (read-line fd) (read-line fd)))
                          ((delim ln))
                        (display ln port) (newline port))))))
            (#t (display "file not found\n"))))))

(define*-public (help obj #&optional (port (current-output-port)))
  "Print all possible documentation for string or symbol."
  (display " *** documentation for `") (display obj) (display "':\n\n")
  (documentation obj port)
  (let ((bb (symbol-binding #f (if (string? obj) (string->symbol obj) obj))))
    (cond ((procedure? bb)
           (display "\n *** procedure-documentation for `")
           (display obj) (display "':\n\n")
           (with-output-to-port port
             (lambda () (procedure-documentation bb))))))
  (display "\n\n"))

;; For testing...
;; (documentation "window-position")
;; (documentation "make-cl-constraint")
;; (apropos-internal "")
