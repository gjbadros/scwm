;; $Id$
;; (C) 1998 Greg J. Badros
;; 4-October-1998

(define-module (app scwm cached-program-exists))

;; Switch this to #t if you're having programs
(define debug-program-cache #f)

(define programs-that-exist #f)

(define-public (initialize-programs-that-exist)
  "Initializes the cache with programs that exist in the current $PATH.
This creates a list that `cached-program-exists?' then checks when
queried whether a program exists or not.  Currently, this procedure
spawns a zsh process to get the list of files in the $PATH very quickly."
  (if (= 0 (system "which zsh >/dev/null"))
      (let ((progs-pipe (open-input-pipe
			 "zsh -fc 'print -l $^path/*(N:t)'")))
	(set! programs-that-exist 
	      (do ((prg (read-line progs-pipe) (read-line progs-pipe))
		   (prgs '() (cons prg prgs)))
		  ((eof-object? prg) prgs)))
	(close-pipe progs-pipe))))

(if (not programs-that-exist)
    (initialize-programs-that-exist)
    (if (not programs-that-exist)
	(display "Failed to initialize list of programs from $PATH using zsh")))

(if debug-program-cache
    (define-public (cached-program-exists? program-name)
      "Return #t if PROGRAM-NAME is in the cache of programs that exist.
Returns #f otherwise.  Debugging is enabled so a message will print
to stdout on hits and misses."
      (if programs-that-exist
	  (if (member program-name programs-that-exist) 
	      (begin (display "hit ") (display program-name) (newline) #t)
	      (begin (display "miss ") (display program-name) (newline) #f))
	  (begin
	    (display "using which") (newline)
	    (= 0 (system (string-append "which " program-name " >/dev/null" ))))))
    (define-public (cached-program-exists? program-name)
      "Return #t if PROGRAM-NAME is in the cache of programs that exist.
Returns #f otherwise."
      (if programs-that-exist
	  (if (member program-name programs-that-exist) #t #f)
	  (= 0 (system (string-append "which " program-name " >/dev/null" ))))))
