;;; $Id$
;;; doc.scm
;;; (C) 1998 Sam Steingold, Greg J. Badros, and Maciej Stachowiak




(define-module (app scwm doc)
  :use-module (ice-9 regex)
  :use-module (app scwm base)
  :use-module (app scwm flux)
  :use-module (app scwm optargs))



;; FIXGJB: hack to put doc-files in root module
(define-public doc-files
  (map (lambda (st) (string-append (scwm-path-prefix) "/share/scwm/" st))
       '("scwm-procedures.txt" "scwm-variables.txt" "scwm-hooks.txt"
         "scwm-concepts.txt" "cassowary_scm-procedures.txt")))

(define-public documentation-debug #f)

(define*-public (documentation func #&optional (port (current-output-port)))
  "Print the documentation for the string or symbol.
Works by searching through the files listed in `doc-files'.
Returns #t if any documentation was found, #f otherwise."
  (let* ((func (if (string? func) func (symbol->string func)))
         (head (string-append "(" func))
         (len (string-length head))
         (delim? (lambda (st) (and (= 1 (string-length st))
                                   (char=? (string-ref st 0) #\np)))))
    (do ((fl doc-files (cdr fl)) (done #f) (fd #f))
        ((or (null? fl) done)
         (if (not done) (write-all port "No documentation for `" func "'\n"))
         done)
      (if documentation-debug (write-all port "trying `" (car fl) "'..."))
      (cond ((file-exists? (car fl))
             (if documentation-debug (display "file exists\n" port))
             (set! fd (open-input-file (car fl)))
             (do ((ln (read-line fd)))
                 ((or (eof-object? ln) done) (close-input-port fd))
               (cond ((and (delim? ln)
                           (begin (set! ln (read-line fd))
                                  (not (eof-object? ln)))
                           (or (and (< len (string-length ln))
                                    (string=? head (substring ln 0 len))
                                    (string-index " )" (string-ref ln len)))
                               (string=? func ln)))
                      (set! done #t)
                      (display ln port) (newline port)
                      (do ((ln (read-line fd) (read-line fd)))
                          ((delim? ln))
                        (display ln port) (newline port)))
                     ((set! ln (read-line fd))))))
            (documentation-debug (display "file not found\n" port))))))

(define*-public (help obj #&optional (port (current-output-port)))
  "Print all possible documentation for string or symbol."
  (display " *** documentation for `" port)
  (display obj port)
  (display "':\n\n" port)
  (documentation obj port)
  (let ((bb (symbol-binding #f (if (string? obj) (string->symbol obj) obj))))
    (cond ((procedure? bb)
           (display "\n *** procedure-documentation for `" port)
           (display obj port) (display "':\n\n" port)
           (with-output-to-port port
             (lambda () (procedure-documentation bb))))))
  (display "\n\n" port))

;; For testing...
;; (documentation "window-position")
;; (documentation "make-cl-constraint")
;; (apropos-internal "")
