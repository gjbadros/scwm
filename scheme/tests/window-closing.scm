

(define (goodbye win)
  (write (X-property-get win "WM_CLASS"))
  (display "\n")
  )

(define (dest-goodbye winid)
  (display "destroyed ")
  (display winid)
  (display "\n")
  )

(define (unmap-goodbye win)
  (display "unmapped ")
  (display (window-class win))
  (display "\n")
  )

(define (close-goodbye win)
  (display "close ")
  (display (window-class win))
  (display "\n")
  )


(add-hook! window-leave-hook goodbye)
(add-hook! window-close-hook close-goodbye)
(add-hook! X-DestroyNotify-hook dest-goodbye)
(add-hook! X-UnmapNotify-hook unmap-goodbye)
