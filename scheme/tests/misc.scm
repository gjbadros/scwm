;; $Id$

(test-case "Read out the X cut buffer contents"
 (X-cut-buffer-string)
 #t)

(test-case "Read out the X cut buffer property"
 (X-property-get 'root-window "CUT_BUFFER1")
 #t)

(test-case "Rotate the X cut buffers"
 (X-rotate-cut-buffers 1)
 #t)

(test-case "Determine the current user's name."
 (user-name)
 => (getenv "LOGNAME"))

(test-case "Determine the current user's homedir."
 (user-home)
 => (getenv "HOME"))

;; (flash-window (current-window-with-focus))
;;(set-window-highlight-background! "navy")

(get-window-colors (select-window-interactively))
(get-window-highlight-colors (select-window-interactively))

