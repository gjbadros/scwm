;; $Id$
;; Copyright (C) 1998, 1999, 2000 Greg J. Badros
;; 4-October-1998

(define-module (app scwm path-cache)
  :use-module (ice-9 string-fun)
  :use-module (app scwm base))

(if (> guile-version 1.3)
    (use-modules (ice-9 popen)))

;; Switch this to #t if you're having problems
(define-public debug-program-cache #f)

(define-public programs-that-exist #f)

(define-public (initialize-programs-that-exist)
  "Initializes the cache with programs that exist in the current $PATH.
This creates a list that `cached-program-exists?' then checks when
queried whether a program exists or not.  Currently, this procedure
spawns a zsh process to get the list of files in the $PATH very quickly."
  (if (program-exists? "zsh")
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
	(display "Failed to initialize list of programs from $PATH using zsh\n")))

(define-public (cached-program-exists? program-name)
  "Return #t if PROGRAM-NAME is in the cache of programs that exist.
Returns #f otherwise.  If `debug-program-cache' is true, a message will 
print to stdout on hits and misses.  You must call 
`initialize-programs-that-exist' before calling this function; otherwise,
it reverts to the (inefficient) implementation of `program-exists?'."
  (if debug-program-cache
      (if (and programs-that-exist (not (string-contains-slash? program-name)))
	  (if (member program-name programs-that-exist) 
	      (begin (display "hit ") (display program-name) (newline) #t)
	      (begin (display "miss ") (display program-name) (newline) #f))
	  (program-exists? program-name))
      (if (and programs-that-exist (not (string-contains-slash? program-name)))
	  (if (member program-name programs-that-exist) #t #f)
	  (program-exists? program-name))))
