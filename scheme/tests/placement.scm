;;(window-style "Netscape: Find" #:other-proc place-near-pointer)
(window-style "Netscape: Find" #:placement-proc (thunk place-near-pointer))
(window-style "findDialog_popup" #:placement-proc (thunk place-near-pointer))

(let ((w (select-window-interactively)))
  (place-near-pointer w))

(window-style "xlogo" #:placement-proc (thunk place-near-pointer))
(window-style "xlogo" #:placement-proc (thunk place-interactively))
