;;; $Id$
;;; test pre-command-hook, post-command-hook

(define (debug-pre-command proc args)
  (display "calling ")
  (write proc)
  (write args)
  (newline))

(define (debug-post-command proc args)
  (display "called ")
  (write proc)
  (write args)
  (newline))

(define (kill-interactive-move-selected-group-or-window proc args)
  (if (eq? proc interactive-move-selected-group-or-window)
      (set! this-command noop)))

(add-hook! pre-command-hook debug-pre-command)
(append-hook! pre-command-hook kill-interactive-move-selected-group-or-window)
(append-hook! post-command-hook debug-post-command)

