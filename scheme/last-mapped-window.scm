;;;; $Id$
;;;; Copyright (C) 1999, 2000 Greg J. Badros

(define-module (app scwm last-mapped-window)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm focus-stack)
  :use-module (app scwm winops))

(define last-mapped-window-internal #f)

(define (save-last-mapped-window win)
  (set! last-mapped-window-internal win))

(add-hook! X-MapRequest-hook save-last-mapped-window)

(define-public (last-mapped-window)
  "Return the window that was mapped most recently.
Returns #f if the most recently mapped window is already
gone. See also `focus-last-mapped-window'."
  (if (window-valid? last-mapped-window-internal)
      last-mapped-window-internal
      #f))

(define*-public (focus-last-mapped-window #:optional (push-focus? #t))
  "Focus and warp to the window that was mapped most recently.
See also `last-mapped-window'."
  (interactive)
  (if push-focus?
      (push-focus-window))
  (let ((w (last-mapped-window)))
    (and w (focus-change-warp-pointer w))))
