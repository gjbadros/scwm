;; $Id$

(define (bind-two-modifier-key-events keycode1 mod1 keycode2 mod2 proc-press proc-release)
  ;; first handle keycode1 last 
  (bind-keycode 'all keycode1 mod2 proc-press #f)
  (bind-keycode 'all keycode1 (+ mod2 mod1) #f proc-release)
  ;; now handle keycode2 last
  (bind-keycode 'all keycode2 mod1 proc-press #f)
  (bind-keycode 'all keycode2 (+ mod1 mod2) #f proc-release))


(define (bind-three-modifier-key-events keycode1 mod1 keycode2 mod2 keycode3 mod3 
					proc-press proc-release)
  (let ((all-mods (+ mod1 mod2 mod3)))
    ;; first handle keycode1 last
    (bind-keycode 'all keycode1 (+ mod2 mod3) proc-press #f)
    (bind-keycode 'all keycode1 all-mods #f proc-release)
    ;; now handle keycode2 last
    (bind-keycode 'all keycode2 (+ mod1 mod3) proc-press #f)
    (bind-keycode 'all keycode2 all-mods #f proc-release)
    ;; now handle keycode 3 last
    (bind-keycode 'all keycode3 (+ mod1 mod2) proc-press #f)
    (bind-keycode 'all keycode3 all-mods #f proc-release)
    ))

(define (car-or-255 l)
  (if (and (list? l) (not (eq? l '()))) (car l) 255))

(begin
  (define XK_CONTROL_L (car-or-255 (keysym->keycode "Control_L")))
  (define XK_META_L (car-or-255 (keysym->keycode "Meta_L")))
  (define XK_ALT_L (car-or-255 (keysym->keycode "Alt_L")))
  (define XK_SHIFT_L (car-or-255 (keysym->keycode "Shift_L")))
  (define XK_HYPER_L (car-or-255 (keysym->keycode "Hyper_L")))
  (define XK_SUPER_L (car-or-255 (keysym->keycode "Super_L")))
  )

(bind-two-modifier-key-events 
 XK_CONTROL_L (mod-mask-control)   XK_META_L (mod-mask-meta)
;; 37 4 64 8 ;; control + meta (labelled control + alt on my msnatkbd)
 (lambda () (move-window 5 5 (current-window-with-focus)))
 (lambda () (move-window 10 10 (current-window-with-focus))))


(bind-three-modifier-key-events 
 XK_CONTROL_L (mod-mask-control)   XK_ALT_L (mod-mask-alt)  XK_SHIFT_L (mod-mask-shift)
;; 37 4 115 16 50 1 ;; control + alt + shift (labelled control + windows + shift on my msnatkbd)
 (lambda () (move-window 30 30 (current-window-with-focus)))
 (lambda () (move-window 50 50 (current-window-with-focus))))
			      
;; Highlight the window about to be effected by the mouse move/resize
(bind-three-modifier-key-events 
 XK_CONTROL_L (mod-mask-control)   XK_SHIFT_L (mod-mask-shift)  XK_META_L (mod-mask-meta)
 (lambda () (set-window-highlight-background! "red" (current-window-with-pointer)))
 (lambda () (set-window-highlight-background! "navyblue" (current-window-with-pointer))))
