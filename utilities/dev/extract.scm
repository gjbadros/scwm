#!/bin/sh
exec guile -l $0 -- --run-from-shell "$@"
!#
;;; extract.scm
;;; Copyright (C) 1998, Harvey J. Stein, hjstein@bfr.co.il
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.GPL.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA

;;; Extracts doc strings from C code which follows scwm conventions:
;;; 1. Procs to document have declarations OTF:
;;;    SCWM_PROC(c_name,
;;;	      "scheme_name",
;;;	      number_of_args,
;;;	      another_number,
;;;	      another_number,
;;;	      (SCM arg1, SCM arg2, ...))
;;; 2. The documentation for said proc starts on the line following it's
;;;    definition, starting with /** & ending with */.
;;; 3. Additional documentation starts with /** spaces identifier: & ends with */, but is not
;;;    preceeded by a SCWM_PROC.
   
;;; Usage:
;;;   Parsing:
;;;   (extract-docs-from-files f1.c f2.c ...)
;;;      Returns a list of doc records extracted documentation from the listed
;;;      files.  Each record is either (DOC doclist-record filename linenumber)
;;;      or (SCWM_PROC procdoc-record filename linenumber).
;;;      A procdoc-record is a list otf:
;;;         (proc-record doclist-record)
;;;      A proc-record is a list otf:
;;;         (c_name "scheme_name" number_of_args another_number another_number ((SCM arg1) (SCM arg2) ...))
;;;      A doclist-record is a list of strings otf:
;;;         (line1 line2 line3 ...)
;;;
;;;   Checking docs are well formed:
;;;   (check-docs doclist)
;;;      Verifies stuff such as number of args = args
;;;      in argslist, all args are SCM types, each arg is mentioned in
;;;      the documentation, etc.
;;;
;;;   Generating procedures-list documentation:
;;;   (docs->proclist doclist)
;;;      Output procedures-list stuff
;;;
;;;   Generating sgml output:
;;;   (docs->sgml doclist)
;;;      Does various passes on the extract-docs-from-files output to
;;;      generate the sgml output.
;;;
;;; Note:
;;; 1. Miscellaneous breaking of abstraction layers needs to be fixed
;;;    - need make-proc-record, make-doc-record, ... & need to remove
;;;      usage of car/list-ref/cdr/... on these things (if I'm doing this).
;;; 2. Too much hacking has lead to a need for some code cleanup.
;;; 3. Still can't load slib stuff without stupid hacks that should
;;;    have been dealt with when guile installed.

(if (not (member "/usr/lib" %load-path))
    (set! %load-path (cons "/usr/lib" %load-path))) ; HACK for guile to find slib!!!!!!!
(use-modules (ice-9 regex)		; For regexp-quote & substitute.
	     (ice-9 slib))		; For sort.
(require 'sort)

(define proc-start-match
  (make-regexp "^[ \t]*SCWM_PROC[ \t]*\\("))
(define doc-start-match
  (make-regexp "^[ \t]*/\\*\\*[ \t]*([^ \t:*]*:.*)")) ; spaces /**[^space or *]
(define post-proc-doc-start-match
  (make-regexp "^[ \t]*/\\*\\*[ \t]+(.*)")) ; spaces /** space+
(define func-name-match
  (make-regexp "^[ \t]*#[ \t]*define[ \t]+FUNC_NAME[ \t]+([^ \t]*)[ \t]*$"))
(define doc-end-match
  (make-regexp "[ \t]*\\*/[ \t]*"))	; Strip off spaces */ spaces

(define (proc:c-name procrec)
  (list-ref procrec 0))

(define (proc:scheme-name procrec)
  (list-ref procrec 1))

(define (proc:required-args procrec)
  (list-ref procrec 2))

(define (proc:optional-args procrec)
  (list-ref procrec 3))

(define (proc:extra-var-args procrec)
  (list-ref procrec 4))

(define (proc:args procrec)
  (list-ref procrec 5))


(define (procdoc:decl procdocrec)
  (list-ref procdocrec 0))

(define (procdoc:doc procdocrec)
  (list-ref procdocrec 1))

(define (procdoc:funcname procdocrec)
  (list-ref procdocrec 2))


(define (docitem:type rec)
  (list-ref rec 0))

(define (docitem:data rec)
  (list-ref rec 1))

(define (docitem:file rec)
  (list-ref rec 2))

(define (vdocitem:decl rec)
  (procdoc:decl (docitem:data rec)))

(define (vdocitem:scheme-name rec)
  (proc:scheme-name (vdocitem:decl rec)))

(define (vdocitem:c-name rec)
  (proc:c-name (vdocitem:decl rec)))

(define (docitem:line rec)
  (list-ref rec 3))

(define (doc:chapter rec)
  (list-ref rec 0))

(define (doc:section rec)
  (list-ref rec 1))

(define (doc:doc rec)
  (list-ref rec 2))


(define (counting-read-line p)
  (set-port-line! p (1+ (port-line p)))
  (read-line p))

;;; Output procedures-list document.
(define (docs->proclist l)
  (for-each proc->list
	    (sort (select-type 'SCWM_PROC l)
		  scheme-name-ci<?)))

(define (scheme-name-ci<? x y)
  (string-ci<? (proc:scheme-name (procdoc:decl (docitem:data x)))
	       (proc:scheme-name (procdoc:decl (docitem:data y)))))

(define (scheme-name<? x y)
  (string<? (proc:scheme-name (procdoc:decl (docitem:data x)))
	    (proc:scheme-name (procdoc:decl (docitem:data y)))))

(define (select-type type docitemlist)
  (let loop ((l docitemlist)
	     (r '()))
    (cond ((null? l) (reverse r))
	  ((eq? (docitem:type (car l)) type)
	   (loop (cdr l) (cons (car l) r)))
	  (else (loop (cdr l) r)))))

(define (displayl . args)
  (for-each display args))

(define (complain file line . complaints)
  (apply displayl file ":" line ": " complaints)
  (display ".\n"))

(define (check-docs docitemlist)
  ;; Do the per-item checks.
  (for-each check-docitem docitemlist)
  ;; Now the global checks.

  ;; Check for >=2 identical scheme names.
  (let loop ((spl (group (sort (select-type 'SCWM_PROC docitemlist)
			       scheme-name<?)
			 (lambda (x y)
			   (string=?
			    (vdocitem:scheme-name x)
			    (vdocitem:scheme-name y))))))
    (for-each
     (lambda (g)
       (if (null? g)
	   (complain "" "" "Internal error - null group")
	   (for-each (lambda (r)
		       (complain (docitem:file r)
				 (docitem:line r)
				 "Scheme name " (vdocitem:scheme-name r) " redefined."
				 "  First defined in " (docitem:file (car g)) " on line "
				 (docitem:line (car g))))
		     (cdr g))))
     spl))
  ;; Don't need to check for >=2 identical proc names - compiler will catch it.
  )

;;; Complains about docitem recs it doesn't like
(define (check-docitem procdocrec)
  (let* ((data     (docitem:data procdocrec))
	 (file     (docitem:file procdocrec))
	 (line     (docitem:line procdocrec)))
    (case (docitem:type procdocrec)
      ((DOC)
      ;; Check that DOC has a nonempty chapter name.
      (if (not (doc:chapter data))
	  (complain file line
		    "Untagged embedded documentation"))

      ;; Check that DOC has a nonempty section name.
      (if (string=? "" (doc:section data))
	  (complain file line
		    "Empty section heading"))

      ;; Check that DOC doc has nonempty documentation.
      (if (null? (doc:doc data))
	  (complain file line
		    "Empty documentation")))

      ;; Check that DOC doc identifier is HOOK or CONCEPT?      

      ((SCWM_PROC)
       (let ((procrec  (procdoc:decl data))
	     (docrec   (procdoc:doc  data))
	     (funcname (procdoc:funcname data)))
	 ;; Check c-name vs scheme-name.
	 (if (and (not (string=? (c-ident->scheme-ident (symbol->string (proc:c-name procrec)))
				 (proc:scheme-name procrec)))
		  (not (string=? (c-name->scheme-name (symbol->string (proc:c-name procrec)))
				 (proc:scheme-name procrec))))
	     (complain file line
		       "Scheme name of " (proc:scheme-name procrec) " doesn't match a C name of "
		       (proc:c-name procrec)))

	 ;; What's this business about the 1st doc line being a "purpose" sentence?
	 ;; What's this business about "leading spaces" being omitted?

	 ;; Check that arg 2+3+4 = length of arg5
	 (if (not (= (+ (proc:required-args procrec)
			(proc:optional-args procrec)
			(proc:extra-var-args procrec))
		     (length (proc:args procrec))))
	     (complain file line
		       "Argument count mismatch in "
		       (proc:scheme-name procrec)))

	 ;; Warn about var param != 0 or 1.
	 (if (and  (not (= (proc:extra-var-args procrec) 0))
		   (not (= (proc:extra-var-args procrec) 1)))
	     (complain file line
		       "Var count is not 0 and is not 1"))

	 ;; Check that all args are of type SCM
	 (let loop ((i 1)
		    (args (proc:args procrec)))
	   (cond ((null? args))
		 ((eq? (caar args) 'SCM)
		  (loop (1+ i) (cdr args)))
		 (else
		  (complain file line
			    "In the declaration for "
			    (proc:scheme-name procrec)
			    ", argument " i " (" (cadar args) ") is not of type SCM"))))

	 ;; Check that the proc is documented:
	 (if (null? docrec)
	     (complain file line
		       "Procedure " (proc:scheme-name procrec) " is not documented"))

	 ;; Check that each arg appears in upper case in description.
	 (let next-arg ((argregexp (map (lambda (arg)
					  (delimited-regexp 
					   (string-upcase! (c-name->scheme-name (symbol->string (cadr arg))))))
					(proc:args procrec)))
			(args (map cadr (proc:args procrec)))
			(i 1))

	   (cond ((null? args))
		 (else
		  (let next-docline ((doc docrec))
		    (cond ((null? doc)
			   (complain file line
				     "Argument " i " (" (car args) ") of "
				     (proc:scheme-name procrec)
				     " is undocumented"))
			  ((regexp-exec (car argregexp) (car doc)))
			  (else (next-docline (cdr doc)))))
		  (next-arg (cdr argregexp) (cdr args) (+ i 1)))))

	 ;; Check for upper case words that don't match args.
	 (let ((args (map (lambda (arg) (string-upcase! (c-name->scheme-name (symbol->string (cadr arg)))))
			  (proc:args procrec))))
	   (for-each (lambda (word)
		       (if (and (upper-case? word)
				(> (string-length word) 1)
				(not (all-special? word))
				(not (member word args)))
			   (complain file line "Documentation for procedure " (proc:scheme-name procrec)
				     " contains upper case word " word " which isn't an argument")))
		     (apply append (map extract-words docrec))))

	 ;; Check that there's a func_name & it matches the c-name
	 (if funcname
	     (if (not (string=? (string-append "s_" (symbol->string (proc:c-name procrec)))
				funcname))
		 (complain file line
			   "Procedure " (proc:scheme-name procrec) " doesn't have a matching FUNC_NAME"))
	     (complain file line
		       "Procedure " (proc:scheme-name procrec) " doesn't have a FUNC_NAME"))))
      (else (complain file line "Internal error - unrecognized doc record type.")))))

(define (upper-case? s)
  (let loop ((i 0)
	     (l (string-length s)))
    (cond ((>= i l) #t)
	  ((and (not (char-upper-case? (string-ref s i)))
		(char-lower-case? (string-ref s i)))
	   #f)
	  (else (loop (1+ i) l)))))

(define (all-special? s)
  (let loop ((i 0)
	     (l (string-length s)))
    (cond ((>= i l) #t)
	  ((and (not (char-upper-case? (string-ref s i)))
		(not (char-lower-case? (string-ref s i))))
	   (loop (1+ i) l))
	  (else #f))))

(define (extract-words s)
  (define (skip i l result)
    (cond ((>= i l) result)
	  ((delimiter? (string-ref s i))
	   (skip (1+ i) l result))
	  (else (grab i (1+ i) l result))))
  (define (grab start end l result)
    (cond ((>= end l) (cons (substring s start end) result))
	  ((delimiter? (string-ref s end))
	   (skip (1+ end) l (cons (substring s start end) result)))
	  (else (grab start (1+ end) l result))))
  (skip 0 (string-length s) '()))

(define (delimiter? c)
  (case c
    ((#\: #\space #\tab #\+ #\= #\\ #\| #\{ #\} #\[ #\] #\' #\` #\" #\: #\; #\. #\/ #\< #\> #\@ #\# #\% #\^ #\& #\* #\( #\) #\,) #t)
    (else #f)))

(define (c-ident->scheme-ident s)
  (define to-regexp (make-regexp "_to_"))
  (define (to->-> s)
    (let ((match (regexp-exec to-regexp s)))
      (if match
	  (regexp-substitute #f match 'pre "->" 'post)
	  s)))
  (c-name->scheme-name (to->-> s)))

(define (c-name->scheme-name s)
  (let* ((normname (map (lambda (c)
			  (if (char=? c #\_) #\- c))
			(string->list s)))
	 (revname (reverse normname)))
    (cond ((or (null? revname)
	       (null? (cdr revname)))
	   (list->string normname))
	  ((and (char=? (car revname) #\p)
		    (char=? (cadr revname) #\-))
	   (list->string (reverse (cons #\? (cddr revname)))))
	  ((and (char=? (car revname) #\x)
		    (char=? (cadr revname) #\-))
	   (list->string (reverse (cons #\! (cddr revname)))))
	  (else
	   (list->string normname)))))
	  

(define (delimited-case-insensitive-regexp s)
  (let ((ci-name (regexp-quote s)))
    (make-regexp
     (string-append "[ \t'`.,:\"]" ci-name "[ \t'`.,:\"]|"
		    "^" ci-name "[ \t'`.,:\"]|"
		    "[ \t'`.,:\"]" ci-name "$|"
		    "^" ci-name "$")
     regexp/icase)))

(define (delimited-regexp s)
  (let ((ci-name (regexp-quote s)))
    (make-regexp
     (string-append "[ \t'`.,:\"]" ci-name "[ \t'`.,:\"]|"
		    "^" ci-name "[ \t'`.,:\"]|"
		    "[ \t'`.,:\"]" ci-name "$|"
		    "^" ci-name "$"))))


;;; ispell crap
(define (ispell-start)
  (system "rm /tmp/ispell-input 2>/dev/null")
  (system "mkfifo /tmp/ispell-input 2>/dev/null")
  (let ((ispell-out (open-input-pipe "ispell -a </tmp/ispell-input"))
	(ispell-in  (open-output-file "/tmp/ispell-input")))
    (read-line ispell-out)
    (cons ispell-in ispell-out)))

(define (ispell-ignore words ports)
  (for-each (lambda (word)
	      (display "@" (car ports))
	      (display word (car ports))
	      (newline (car ports)))
	    words))

(define (ispell-send line ports)
  (display (ispell-escape line) (car ports))
  (newline (car ports))
  (flush-all-ports)
  (let loop ((resp (read-line (cdr ports))))
    (flush-all-ports)
    (cond ((string=? resp "") '())
	  (else (cons resp (loop (read-line (cdr ports))))))))

(define *scwm-ok-words*
  '(scwm fvwm hilight viewport scwmexec scwmrepl menuitem
	  menuitems hotkey submenu colormap 
	  pseudocolor staticgray staticcolor grayscale directcolor truecolor
	  scwmrc reallysmart smartplacement pposition mwm mwm alt meta hyper
	  broadcastinfo smartplacementisreallysmart randomplacement
	  super car cdr cadr titlebar unhover bg fg popup iconify
	  iconifying deiconify deiconifying unmap iconified desktop desktops
	  honoured lenience xproperty xored
	  shift control meta alt hyper super callbacks decors viewports))

(define (ispell-report io)
  (cond ((null? io) '())
	(else (case (string-ref (car io) 0)
		((#\* #\+ #\-) (ispell-report (cdr io)))
		((#\&) (cons (cons "Misspelling : "
				   (ispell-find-word (car io)))
			     (ispell-report (cdr io))))
		((#\? #\#) (cons (cons "Unrecognized word : "
				       (ispell-find-word (car io)))
				 (ispell-report (cdr io))))
		(else (cons (cons "Unrecognized ispell msg for : "
				  (ispell-find-word (car io)))
			    (ispell-report (cdr io))))))))

(define (ispell-find-word s)
  (list->string (let loop ((s (cddr (string->list s))))
		  (cond ((null? s) '())
			((char=? (car s) #\space)
			 '())
			(else (cons (car s) (loop (cdr s))))))))

(define (ispell-stop ports)
  (close-port (car ports))
  (close-pipe (cdr ports))
  (system "rm /tmp/ispell-input 2>/dev/null"))


(define (ispell-docs docs)
  (let ((p '()))
    (dynamic-wind
     (lambda ()
       (set! p (ispell-start))
       (ispell-ignore *scwm-ok-words* p))
     (lambda () (for-each (lambda (rec)
			    (ispell-complain rec p))
			  docs))
     (lambda () (ispell-stop p)))))

(define (ispell-complain rec p)
  (for-each (lambda (complaint)
	      (complain (docitem:file rec) (docitem:line rec)
			(car complaint) (cdr complaint)))
	    (apply append (map (lambda (line)
				 (ispell-report (ispell-send line p)))
			       (docitem->plaintextlist rec)))))

(define (ispell-escape s)
  (string-append "^" s))

;;; Outputs procdocrec in format suitable for a procedures-list document.
(define (proc->list procdocrec)
  (if (eq? 'SCWM_PROC (docitem:type procdocrec))
      (let ((procrec (procdoc:decl (docitem:data procdocrec)))
	    (docrec  (procdoc:doc (docitem:data procdocrec))))
	(display (function-call-decl procrec))
	(newline)
	(for-each (lambda (docline) (display docline) (newline))
		  docrec)
	(displayl "[From " (docitem:file procdocrec) ":"
		  (docitem:line procdocrec) "]\n\n\f\n"))))
	   
(define (function-call-decl procrec)
  (apply string-append "("
	 (proc:scheme-name procrec)
	 (let loop ((args (map (lambda (a)
				 (c-name->scheme-name 
				  (symbol->string (cadr a))))
			       (proc:args procrec)))
		    (req  (proc:required-args procrec))
		    (opt  (proc:optional-args procrec))
		    (rest (proc:extra-var-args procrec)))
	   (cond ((null? args)
		  '(")"))
		 ((> req 0)
		  (cons " " (cons (car args)
				  (loop (cdr args) (- req 1) opt rest))))
		 ((and (= req 0)
		       (> opt 0))
		  (cons " #&optional" (loop args (- req 1) opt rest)))
		 ((and (< req 0)
		       (> opt 0))
		  (cons " " (cons (car args)
				  (loop (cdr args) req (- opt 1) rest))))
		 ;; Now know req <= 0 & opt <= 0.
		 ((> rest 0)
		  (cons " . " (cons (car args)
				    (loop (cdr args) req opt 0))))
		 (else
		  (cons " " (cons (car args)
				  (loop (cdr args) -1 0 0))))))))
		 
;;; Output doc record in format suitable for ispell:
(define (docitem->plaintext procdocrec)
  (let* ((file (docitem:file procdocrec))
	 (line (docitem:line procdocrec)))
    (for-each (lambda (d) (complain file line d))
	      (docitem->plaintextlist procdocrec))))

(define (docitem->plaintextlist procdocrec)
  (case (docitem:type procdocrec)
    ((SCWM_PROC) (procdoc:doc (docitem:data procdocrec)))
    ((DOC)       (doc:doc (docitem:data procdocrec)))
    (else (complain (docitem:file procdocrec) (docitem:line procdocrec)
		    "Internal error - unrecognized doc record type of " (docitem:type procdocrec))
	  '())))

(define (docs->text docs)
  (for-each (lambda (rec) (for-each (lambda (d) (displayl d "\n"))
				    (docitem->plaintextlist rec)))
	    docs))

(define (docs->annotated-text docs)
  (for-each docitem->plaintext
	    docs))


;;; Extract docs from specified files.  Return list of procdoc
;;; records.
(define (extract-docs-from-files . files)
  (let loop ((defs '())
	     (files files))
    (cond ((null? files) (reverse defs))
	  (else (loop (call-with-input-file (car files)
			(lambda (p) (extract-docs-from-port p defs)))
		      (cdr files))))))

;;; Extract docs from specified input port.
(define (extract-docs-from-port p . start)
  (let ((filename (port-filename p)))
    (let loop ((line (counting-read-line p))
	       (defs (if (null? start) '() (car start))))
      (if (eof-object? line)
	  defs
	  (let* ((proc (regexp-exec proc-start-match line))
		 (docstart (or proc (regexp-exec doc-start-match line)))
		 (linenum (port-line p)))
	    (cond (proc
		   (let ((doc (extract-proc-n-doc line p)))
		     (cond (doc
			    (loop (counting-read-line p)
				  (cons (list 'SCWM_PROC
					      doc
					      filename
					      linenum)
					defs)))
			   (else
			    (complain filename linenum "SCWM_PROC not parsable")
			    (loop (counting-read-line p)
				  defs)))))
		  (docstart
		   (let ((doc (parse-doc (extract-doc p line))))
		     (loop (counting-read-line p)
			   (cons (list 'DOC
				       doc
				       filename
				       linenum)
				 defs))))
		  (else
		   (loop (counting-read-line p) defs))))))))

(define (next-non-whitespace-line p)
  (define whitespace-line (make-regexp "^[ \t]*$"))
  (let ((line (counting-read-line p)))
    (if (or (eof-object? line)
	    (regexp-exec whitespace-line line))
	(next-non-whitespace-line p)
	line)))
	
(define (extract-proc-n-doc line p)
  (let* ((proc (parse-proc (match-parentheses line p)))
	 (next (counting-read-line p)))
    (cond ((not proc)			; Proc not parsable
	   #f)
	  ((eof-object? next)		; No doc & no func
	   (list proc '() #f))
	  ((regexp-exec post-proc-doc-start-match next)	; Doc is first.
	   (let* ((doc (extract-doc p next post-proc-doc-start-match))
		  (next (next-non-whitespace-line p))
		  (match (if next (regexp-exec func-name-match next) #f)))
	     (if match
		 (list proc doc (substring (vector-ref match 0)
					   (car (vector-ref match 2))
					   (cdr (vector-ref match 2))))
		 (list proc doc #f))))
	  (else
	   (let* ((match (regexp-exec func-name-match next)) ; Func name must be next.
		  (next (next-non-whitespace-line p))
		  (doc (extract-doc p next post-proc-doc-start-match))) ; Then the docs.
	     (if match
		 (list proc doc (substring (vector-ref match 0)
					   (car (vector-ref match 2))
					   (cdr (vector-ref match 2))))
		 (list proc doc #f)))))))

(define (extract-doc p . xargs)
  (define (doclist lines)
    lines)
  (define (extract-to-end lines)
    (if (eof-object? (car lines))
	(doclist (reverse lines))
	(let ((end (regexp-exec doc-end-match (car lines))))
	  (if end
	      (doclist (reverse (cons (substring (car lines) 0 (car (vector-ref end 1)))
				      (cdr lines))))
	      (extract-to-end (cons (counting-read-line p) lines))))))

  (let ((line (if (null? xargs)
		  (counting-read-line p)
		  (car xargs)))
	(doc-start-match (if (or (null? xargs)
				 (null? (cdr xargs)))
			     doc-start-match
			     (cadr xargs))))
    (if (eof-object? line)
	'()
	(let ((start (regexp-exec doc-start-match line)))
	  (if start
	      (extract-to-end (list (substring line (car (vector-ref start 2)) (cdr (vector-ref start 2)))))
	      '())))))

;;; FIXME!!!  This is dumb, but it probably works well enough.
(define (match-parentheses line p)
  (dumb-match-parentheses line p))

(define (dumb-match-parentheses line p)
  (let loop ((umc (unmatched-p-count (string->list line)))
	     (lines (list line)))
    (if (> umc 0)
	(let ((line (counting-read-line p)))
	  (if (eof-object? line)
	      (apply string-append (reverse lines))
	      (loop (+ umc (unmatched-p-count (string->list line)))
		    (cons line lines))))
	(apply string-append (reverse lines)))))

(define (unmatched-p-count l)
  (let loop ((c 0)
	     (l l))
    (cond ((null? l) c)
	  ((char=? (car l) #\()
	   (loop (+ c 1) (cdr l)))
	  ((char=? (car l) #\))
	   (loop (- c 1) (cdr l)))
	  ((char=? (car l) #\")
	   (loop c (skip-to-next-quote (cdr l))))
	  (else (loop c (cdr l))))))

(define (skip-to-next-quote l)
  (cond ((null? l) '())
	((char=? (car l) #\")
	 (cdr l))
	((char=? (car l) #\\)		; Escaped quote.
	 (if (and (not (null? (cdr l)))
		  (char=? (cadr l) #\"))
	     (skip-to-next-quote (cddr l))
	     l))
	(else
	 (skip-to-next-quote (cdr l)))))


(define (parse-doc doclist)
  (define parser (make-regexp "^[ \t]*([^ \t:]*):[ \t]*(.*)[ \t]*$"))
  (cond ((null? doclist) '(#f '()))
	(else (let ((match (regexp-exec parser (car doclist))))
		(list (match:substring match 1)
		      (match:substring match 2)
		      (cdr doclist))))))
  

(define (parse-proc defstring)
  (define parser (make-regexp "^[ \t]*SCWM_PROC[ \t]*\\([ \t]*([^, \t]*)[ \t]*,[ \t]*\"([^, \t]*)\"[ \t]*,[ \t]*([^, \t]*)[ \t]*,[ \t]*([^, \t]*)[ \t]*,[ \t]*([^, \t]*)[ \t]*,[ \t]*(\\([^)]*\\)[ \t]*)\\)[ \t]*$"))
  (let ((match (regexp-exec parser defstring)))
    (if match
	(let ((args (list->vector (cdr (split-match match)))))
	  (list (string->symbol (vector-ref args 0))
		(vector-ref args 1)
		(string->number (vector-ref args 2))
		(string->number (vector-ref args 3))
		(string->number (vector-ref args 4))
		(let ((args (with-input-from-string (string-append "(" (replace-occurrences (vector-ref args 5) #\, ")(") ")")
			      read)))
		  (if (equal? args '(()))
		      '()
		      args))))
	#f)))

(define (replace-occurrences string char repl)
  (define (my-repl start end char srepl)
    (cond ((null? end) (list->string (reverse start)))
	  ((char=? (car end) char)
	   (my-repl (append srepl start) (cdr end) char srepl))
	  (else
	   (my-repl (cons (car end) start) (cdr end) char srepl))))
  (my-repl '() (string->list string) char (reverse (string->list repl))))


(define (split-match match)
  (map (lambda (startnend)
	 (substring (vector-ref match 0)
		    (car startnend)
		    (cdr startnend)))
       (cdr (vector->list match))))


(define (stringify value)
  (with-output-to-string 
    (lambda () (write value))))

(define (proc->primitives-ssgml docitem)
  (let* ((data (docitem:data docitem))
	 (proc (procdoc:decl data))
	 (doc (procdoc:doc data)))
    `((refentry (id ,(stringify (proc:c-name proc))))
      ((refnamediv)
       ((refname) ,(proc:scheme-name proc))
       ((refpurpose) ,(car doc)))
      ((refsynopsisdiv)
       ((synopsis) ,(function-call-decl proc)))
      ((refsect1)
       ((title) "Description")
       ((para)  ,@doc))
      ((refsect1)
       ((title) "Implementation Notes")
       ((para) "Defined in "
	       ((ulink (url ,(docitem:file docitem)))
		((filename) ,(docitem:file docitem)))
	       ,(string-append " at line " (stringify (docitem:line docitem))
			       "."))))))

(define (proclist->primitives-chapter l)
  (make-chapter "Primitives in Alphabetical Order"
		(map proc->primitives-ssgml l)))

(define (make-chapter name l)
  `((chapter)
    ((title) ,name)
    ,@l))

(define (lexcmp selectors)
  (lambda (x y)
    (if (null? selectors)
	#t
	(let* ((selector (car selectors))
	       (sel (list-ref selector 0))
	       (less (list-ref selector 1))
	       (eq (list-ref selector 2))
	       (a (sel x))
	       (b (sel y)))
	  (or (less a b)
	      (and (eq a b)
		   ((lexcmp (cdr selectors)) x y)))))))
	     

(define (proclist->file-chapter procs)
  (let ((procs (group (sort procs (lexcmp (list (list (lambda (x) (docitem:file x)) string<? string=?)
						(list (lambda (x) (docitem:line x)) < =))))
		      (lambda (x y) (string=? (docitem:file x) (docitem:file y))))))
    (make-chapter "Primitives by File"
		  (map gen-file-group procs))))

;;; Converts 
;;; (1 1 1 1 2 2 3 3 3 3 ...) to:
;;; ((1 1 1 1) (2 2) (3 3 3 3) ...)
(define (group l eqcmp)
  (define (grp l result)
    (cond ((null? l) (list (reverse result)))
	  ((null? result)
	   (grp (cdr l) (cons (car l) result)))
	  ((eqcmp (car l) (car result))
	   (grp (cdr l) (cons (car l) result)))
	   (else
	    (cons result (grp l '())))))
  (grp l '()))
  
(define (gen-file-group procs-from-file)
  `((sect1)
    ((title) ,(docitem:file (car procs-from-file)))
    ((itemizedlist)
     ,@(map (lambda (rec)
	      `((listitem)
		((para)
		 ((link (linkend ,(proc:c-name (procdoc:decl (docitem:data rec)))))
		  ((function) ,(proc:scheme-name (procdoc:decl (docitem:data rec))))
		  ,(string-append "&mdash; " (car (procdoc:doc (docitem:data rec))))))))
	    procs-from-file))))


(define (emblist->ssgml docs)
  (let ((docs (group (sort docs (lexcmp (list (list (lambda (x) (doc:chapter (docitem:data x)))
						    string-ci<? string-ci=?))))
		     (lambda (x y) (string-ci=? (doc:chapter (docitem:data x))
						(doc:chapter (docitem:data y)))))))
    (map embchapter->ssgml docs)))

(define (embchapter->ssgml group)
  (make-chapter (doc:chapter (docitem:data (car group)))
		(map embsect->ssgml group)))

(define (embsect->ssgml item)
  `((sect1 (id ,(doc:section (docitem:data item))))
    ((title) ,(doc:section (docitem:data item)))
    ((para) ,@(doc:doc (docitem:data item)))))


(define (docs->sgml frontpiece docs)
  (display "<!DOCTYPE Book PUBLIC \"-//Davenport//DTD DocBook V3.0//EN\">\n")
  (sgml (docs->ssgml frontpiece docs)))

(define (docs->ssgml frontpiece docs)
  (let ((procs (sort (select-type 'SCWM_PROC docs) scheme-name-ci<?))
	(embdocs (select-type 'DOC docs)))
    `((book)
      ,frontpiece
      ,(proclist->primitives-chapter procs)
      ,(proclist->file-chapter procs)
      ,@(emblist->ssgml embdocs))))


(define (escape s)
  (list->string
   (let loop ((s (string->list s)))
     (cond ((null? s) '())
	   (else (case (car s)
		   ((#\<) (append '(#\& #\l #\t #\;)
				  (loop (cdr s))))
		   ((#\>) (append '(#\& #\g #\t #\;)
				  (loop (cdr s))))
		   ((#\&) (append '(#\& #\a #\m #\p #\;)
				  (loop (cdr s))))
		   (else (cons (car s) (loop (cdr s))))))))))

;;; Convert ssgml to sgml:
(define (sgml form . depth)
  (if (null? depth) (set! depth '(0)))
  (cond ((string? form)
	 (display (make-string (car depth) #\space))
	 (display (escape form))
	 (newline))
	((null? form)
	 '())
	(else 
	      (display (make-string (car depth) #\space))
	      (sgml-render-start (car form))
	      (for-each (lambda (f) (sgml f (+ (car depth) 3)))
			(cdr form))
	      (display (make-string (car depth) #\space))
	      (sgml-render-end (car form)))))

(define (sgml-render-start tag)
  (displayl "<" (car tag))
  (for-each (lambda (args)
	      (displayl " " (car args) "=")
	      (write (cadr args)))
	    (cdr tag))
  (display ">\n"))

(define (sgml-render-end tag)
  (displayl "</" (car tag) ">\n"))

(define testfilelist
  '("/home/hjstein/software/scwm/scwm/Grab.c"
    "/home/hjstein/software/scwm/scwm/ICCCM.c"
    "/home/hjstein/software/scwm/scwm/add_window.c"
    "/home/hjstein/software/scwm/scwm/binding.c"
    "/home/hjstein/software/scwm/scwm/borders.c"
    "/home/hjstein/software/scwm/scwm/callbacks.c"
    "/home/hjstein/software/scwm/scwm/color.c"
    "/home/hjstein/software/scwm/scwm/colormaps.c"
    "/home/hjstein/software/scwm/scwm/colors.c"
    "/home/hjstein/software/scwm/scwm/complex.c"
    "/home/hjstein/software/scwm/scwm/decor.c"
    "/home/hjstein/software/scwm/scwm/decorations.c"
    "/home/hjstein/software/scwm/scwm/deskpage.c"
    "/home/hjstein/software/scwm/scwm/draw-pie-menu.c"
    "/home/hjstein/software/scwm/scwm/drawmenu.c"
    "/home/hjstein/software/scwm/scwm/errors.c"
    "/home/hjstein/software/scwm/scwm/events.c"
    "/home/hjstein/software/scwm/scwm/face.c"
    "/home/hjstein/software/scwm/scwm/focus.c"
    "/home/hjstein/software/scwm/scwm/font.c"
    "/home/hjstein/software/scwm/scwm/getopt.c"
    "/home/hjstein/software/scwm/scwm/getopt1.c"
    "/home/hjstein/software/scwm/scwm/guile-compat.c"
    "/home/hjstein/software/scwm/scwm/icons.c"
    "/home/hjstein/software/scwm/scwm/image.c"
    "/home/hjstein/software/scwm/scwm/init_scheme_string.c"
    "/home/hjstein/software/scwm/scwm/menu.c"
    "/home/hjstein/software/scwm/scwm/menuitem.c"
    "/home/hjstein/software/scwm/scwm/miscprocs.c"
    "/home/hjstein/software/scwm/scwm/module-interface.c"
    "/home/hjstein/software/scwm/scwm/move.c"
    "/home/hjstein/software/scwm/scwm/placement.c"
    "/home/hjstein/software/scwm/scwm/resize.c"
    "/home/hjstein/software/scwm/scwm/screen.c"
    "/home/hjstein/software/scwm/scwm/scwm.c"
    "/home/hjstein/software/scwm/scwm/scwmmenu.c"
    "/home/hjstein/software/scwm/scwm/shutdown.c"
    "/home/hjstein/software/scwm/scwm/string_token.c"
    "/home/hjstein/software/scwm/scwm/syscompat.c"
    "/home/hjstein/software/scwm/scwm/system.c"
    "/home/hjstein/software/scwm/scwm/util.c"
    "/home/hjstein/software/scwm/scwm/virtual.c"
    "/home/hjstein/software/scwm/scwm/window.c"
    "/home/hjstein/software/scwm/scwm/xmisc.c"
    "/home/hjstein/software/scwm/scwm/xproperty.c"))

(define frontpiece
  `((bookinfo)
    ((title)
     ((productname) "SCWM Reference Manual"))
    ((authorgroup)
     ((author)
      ((firstname) "Maciej")
      ((surname) "Stachowiak")
      ((affiliation)
       ((shortaffil) "MIT")
       ((jobtitle) "M.S. Degree Recipient")
  	  ((orgname) "Massachusetts Institute of Technology")
  	  ((orgdiv) "Department of Computer Science")
  	  ((address)
  	    ((city) "Cambridge")
  	    ((state) "Massachusetts")
  	    ((postcode) "12345")
  	    ((country) "U.S.A.")
  	    ((email) "mstachow@mit.edu"))))
      ((author)
  	((firstname) "Greg")
  	((surname) "Badros")
  	((affiliation)
  	  ((shortaffil) "UWashington")
  	  ((jobtitle) "Graduate Research Assistant")
  	  ((orgname) "University of Washington")
  	  ((orgdiv) "Department of Computer Science and Engineering")
  	  ((address)
  	    ((city) "Seattle")
  	    ((state) "Washington")
  	    ((postcode) "98195")
  	    ((country) "U.S.A.")
  	    ((email) "gjb@cs.washington.edu")))))
    ((releaseinfo) "Release pre-0.8")
    ((pubdate) "28 July 1998")
    ((copyright)
      ((year) "1997&ndash;1998")
      ((holder) "Maciej Stachowiak and Greg J. Badros"))))


(define (usage)
  (displayl "Usage: extract.scm [options] file1.c file2.c ...
  Extracts documentation from specified C source code files.
  Documentation must be embedded according to SCWM conventions:
   - Functions declared with the SCWM_PROC macro will be documented.
     They can be immediately followed by comments of the form /**
     ... */, which will be assumed to document the preceeding
     SCWM_PROC.  Each SCWM_PROC should be followed by a FUNC_NAME
     define which matches the C function name given by the SCWM_PROC.
   - Comments of the form /** chapter_name : section_name ... */ will
     also be extracted.

  Options:
    -c, --check            Check documentation for reasonableness.
    -s file, --sgml file   Generate SGML and output to specified file.
    -p file, --proc file   Generate procedure listing and output to
                           specified file.
    -t file, --text file   Generate plain text output to specified
                           file.
    -a, --annotated-text   Output plain text with each line prefixed by
                           file:line_number:.
    -l, --ispell           Run ispell on documentation.  Currently
                           hangs when given full SCWM source code set.
    -w, --words  'word word ...' More words for ispell to ignore.
    -h, -? --help          Display this info.

  If no flags are given, the default action is to check the files.
"))

(define (process-arg n func arg rest files actions)
;;  (displayl "process-arg\n"
;;	    "arg     : " arg "\n"
;;	    "rest    : " rest "\n"
;;	    "files   : " files "\n"
;;	    "actions : " actions "\n")
  (cond ((= n 0)
	 (process-cmd-line rest files (cons (lambda (docs) (func docs)) actions)))
	((= n 1)
	 (cond ((null? rest)
		(displayl arg
			  " flag given without arguments.  Ignored.\n")
		(process-cmd-line rest files actions))
	       (else (process-cmd-line (cdr rest)
				       files
				       (cons (lambda (docs) (with-output-to-file (car rest)
							      (lambda () (func docs))))
					     actions)))))
	(else
	 (displayl "Internal error: process-arg only takes 0 or 1 as arg count\n"))))

(define (process-cmd-line args files actions)
  (call-with-current-continuation
   (lambda (ret)
     (cond ((null? args)
;;	    (displayl "args    : " args "\n"
;;		      "files   : " files "\n"
;;		      "actions : " actions "\n")
	    (if (null? files)
		(displayl "Error: You must specify at least one file.")
		(let ((docs (apply extract-docs-from-files (reverse files))))
		  (if (null? actions)
		      (check-docs docs)
		      (for-each (lambda (act)
				  (act docs))
				(reverse actions))))))
	   (else 
;;	    (displayl "process-cmd-line: processing '" (car args) "'\n")
	    (case (string->symbol (car args))
		   ((-l --ispell)
		    (process-arg 0 ispell-docs (car args) (cdr args) files actions))
		   ((-c --check)
		    (process-arg 0 check-docs (car args) (cdr args) files actions))
		   ((-s --sgml)
		    (process-arg 1
				 (lambda (d) (docs->sgml frontpiece d))
				 (car args) (cdr args) files actions))
		   ((-p --proc)
		    (process-arg 1 docs->proclist (car args) (cdr args) files actions))
		   ((-t --text)
		    (process-arg 1 docs->text (car args) (cdr args) files actions))
		   ((-a --annotated-text)
		    (process-arg 1 docs->annotated-text (car args) (cdr args) files actions))
		   ((-w --words)
		    (cond ((null? (cdr args))
			   (displayl (car args)
				     " flag given without arguments.  Ignored.\n")
			   (process-cmd-line (cdr args) files actions))
			  (else (set! *scwm-ok-words*
				      (append (extract-words (cadr args))
					      *scwm-ok-words*))
				(process-cmd-line (cddr args) files actions))))
		   ((-h -? --help)
		    (usage)
		    (ret '()))
		   (else
;;		    (displayl "process-cmd-line: else.  (car args) = '" (car args) "'\n")
;;		    (displayl "(eq? (car args) '-i) = " (eq? (string->symbol (car args)) '-i) "\n")
		    (process-cmd-line (cdr args) (cons (car args) files) actions))))))))

;;; Arg processing.
(cond ((or (null? (command-line))
	   (null? (cdr (command-line)))))
      ((null? (cddr (command-line)))
       (usage)
       (exit))
      (else 
       (process-cmd-line (cddr (command-line)) '() '())
       (exit)))
