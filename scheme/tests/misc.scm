;; $Id$

(X-cut-buffer-string)

(X-property-get 'root-window "CUT_BUFFER1")

(X-rotate-cut-buffers 1)

;; (flash-window (current-window-with-focus))
;;(set-window-highlight-background! "navy")

(get-window-colors (select-window-interactively))
(get-window-highlight-colors (select-window-interactively))

