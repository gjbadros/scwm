;; $Id$

(define WM_PROTOCOLS (string->X-atom "WM_PROTOCOLS"))
(define WM_DELETE_WINDOW (string->X-atom "WM_DELETE_WINDOW"))
(define WM_TAKE_FOCUS (string->X-atom "WM_TAKE_FOCUS"))
(define KWM_MODULE_INIT (string->X-atom "KWM_MODULE_INIT"))
(define KWM_MODULE_DESKTOP_CHANGE (string->X-atom "KWM_MODULE_DESKTOP_CHANGE"))
(define KWM_MODULE_DESKTOP_NAME_CHANGE (string->X-atom "KWM_MODULE_DESKTOP_NAME_CHANGE"))
(define KWM_MODULE_DESKTOP_NUMBER_CHANGE (string->X-atom "KWM_MODULE_DESKTOP_NUMBER_CHANGE"))
(define KWM_WIN_ADD (string->X-atom "KWM_WIN_ADD"))


(send-client-message (select-window-interactively) 
		     WM_PROTOCOLS WM_DELETE_WINDOW)

(define kpanel-winid 20971524) ;; use xwininfo to get this

(send-client-message kpanel-winid
		     WM_PROTOCOLS WM_DELETE_WINDOW)

(send-client-message kpanel-winid
		     KWM_MODULE_DESKTOP_CHANGE 2)

(send-client-message kpanel-winid
		     KWM_MODULE_DESKTOP_NAME_CHANGE 2)

(select-window-interactively)


;; (reset-hook! client-message-hook)

(add-hook! client-message-hook (lambda (a f d) (display "c-m-h\n")
				       (display (X-atom->string a))
				       (display ": ")
				       (display f)
				       (write d)
				       (display "\n")))

(send-client-message (+ kpanel-winid 1) KWM_WIN_ADD 37748750)


