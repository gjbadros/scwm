; (load "test.scm")

(define test-group
  (select-window-interactively "Select a window of the group to operate on"))

(test-case "Iconify individually"
 (iconify-group-individually test-group)
 #t)

(test-case "Deiconify"
 (deiconify-group-or-window test-group)
 #t)

(test-case "Iconify group"
 (iconify-group test-group)
 #t)

(test-case "Deiconify group"
 (deiconify-group-or-window test-group)
 #t)

(test-case "Window-shade"
 (window-shade-group test-group)
 #t)

(test-case "Window-unshade"
 (window-unshade-group test-group)
 #t)

(test-case "Stick"
 (stick-group test-group)
 #t)

(test-case "Unstick"
 (unstick-group test-group)
 #t)

(test-case "Keep on top"
 (keep-group-on-top test-group)
 #t)

(test-case "Un-keep on top"
 (un-keep-group-on-top test-group)
 #t)

(test-case "Raise"
 (raise-group test-group)
 #t)

(test-case "Lower"
 (lower-group test-group)
 #t)

(define test-orig-pos (window-viewport-position test-group))

(test-case "Move right"
 (move-group-relative 50 10 test-group)
 #t)

(test-case "Move left"
 (move-group-relative -100 -50 test-group)
 #t)

(test-case "Move to original position"
 (move-group (car test-orig-pos) (cadr test-orig-pos) test-group)
 #t)

(test-case "Move to desk"
 (move-group-to-desk 2 test-group)
 #t)

(test-case "Interactive move"
 (interactive-move-group test-group)
 #t)

(test-case "Close"
 (close-group test-group)
 #t)
