;; $Id$ -*- scwm -*-

(define swi select-window-interactively)

;;(defmacro swi ()
;;  `(select-window-interactively))

(raise-window-above (swi) (swi))

(raise-window (swi))

(restack-windows)

(list-all-windows)


(map (lambda (w)
       (set-window-title! w (window-machine-name w))) 
     (list-windows #:only (lambda (w)
			    (string=? (window-class w) "Emacs"))))


(set-window-text-property (current-window-with-pointer) "WM_NAME" "Foo")

(set-window-title! (current-window-with-pointer) "Foo")

(list-all-windows-in-stacking-order) 

(show-window-list-menu #:warp-to-first #t)

;; We need accessors for window background information,
;; and window-hilight background information
(define* (flash-window win #&optional (color (make-color "red")))
  (set-window-background! color win)
  (add-timer-hook! (sec->usec .5) (lambda () (set-window-background! "grey76" win))))

(for-each (lambda (w) (flash-window w)) (list-all-windows-in-stacking-order))
