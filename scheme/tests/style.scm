(window-style "*" #:show-icon #t #:sticky-icon #f)
(window-style "*" #:show-icon #t #:sticky-icon #t)
(window-style "xterm" #:use-theme (load-theme "gjb"))
(window-style "xterm" #:show-icon #t)
(window-style "*" #:show-icon #t)
(window-style "*" #:show-icon #f)
(window-style "*" #:sticky-icon #t)
(window-style "*" #:random-placement #f #:smart-placement #f)

(window-style "xclock" #:show-icon #t)
(window-style "XTerm" #:show-icon #f)
(window-style "XTerm" #:show-icon #f)
(window-style "*xterm*" #:show-icon #f)

(deiconify-to-current-viewport (select-window-interactively))

(move-window-to-viewport 1 1 (select-window-from-window-list))

(window-style "*" #:border-width 5 #:mwm-border #t)

(window-style "xlogo" #:no-titlebar #t #:plain-border #f)
(window-style "*" #:border-width 6)
(resize-to 200 200)

(window-style "*" #:squashed-titlebar #t)
(window-style "*" #:squashed-titlebar #f)
(window-style "xterm" #:squashed-titlebar #t)
(window-style "xterm" #:squashed-titlebar #f)
(interactive-resize)

(window-style "*" #:bg "purple")
(border-style #:hidden-handles #f #:no-inset #f)

(window-style "foo" #:squashed-titlebar #t)

(raise-window)
(set-object-property! w 'no-side-decorations #t)
(set-object-property! w 'no-side-decorations #f)
(force-reset-window-frame! w)
(window-style "*" #:no-side-decorations #f)
(window-style "XTerm" #:no-side-decorations #t)
(window-style "XTerm" #:no-side-decorations #f)
(window-style "XTerm" #:squashed-titlebar #t)
(window-style "XTerm" #:squashed-titlebar #f)
(squash-titlebar-decoration w)
(unsquash-titlebar-decoration w)
(define w (select-window-interactively))

