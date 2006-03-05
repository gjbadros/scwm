;; $Id$ -*- scwm -*-

(use-modules (app scwm stacking))

(define swi select-window-interactively)

;;(defmacro swi '()
;;  `(select-window-interactively))

(raise-window-above (swi) (swi))

(raise-window (swi))

(restack-windows)

(lower-by-one (current-window-with-pointer))

(list-all-windows)


(map (lambda (w)
       (set-window-title! w (window-machine-name w))) 
     (list-windows #:only (lambda (w)
			    (string=? (window-class w) "Emacs"))))


(X-property-set! (current-window-with-pointer) "WM_NAME" "Foo")

(set-window-title! (current-window-with-pointer) "Foo")

(list-all-windows-in-stacking-order) 

(show-window-list-menu #:warp-to-first #t)

;; We need accessors for window background information,
;; and window-hilight background information
(define* (flash-window win #:optional (color (make-color "red")))
  (set-window-background! color win)
  (add-timer-hook! (sec->usec .5) (lambda () (set-window-background! "grey76" win))))

(for-each (lambda (w) (flash-window w)) (list-all-windows-in-stacking-order))

(define (wildcard->regexp wildcard)
  (regexp-substitute/global 
   #f "\\\\\\*|\\\\\\?" 
   (regexp-quote wildcard) 
   'pre 
   (lambda (match) 
     (case (string-ref (match:string match) (1+ (match:start match))) 
       ((#\*) ".*")
       ((#\?) "."))) 
   'post))


(window-size-hints (current-window-with-focus))
(window-size-hints (select-window-interactively))

(resize-frame-to 400 400)
(resize-to 400 400)
(window-size)
(window-frame-size)

(window-id (select-window-interactively))
(window-id 'root-window)
