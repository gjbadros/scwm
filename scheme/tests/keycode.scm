;; $Id$

(define (bind-two-modifier-key-events modkey1 modkey2 proc-press proc-release)
  (let ((keycode1 (car modkey1))
	(mod1 (cdr modkey1))
	(keycode2 (car modkey2))
	(mod2 (cdr modkey2)))
    ;; first handle keycode1 last 
    (bind-keycode 'all keycode1 mod2 proc-press #f)
    (bind-keycode 'all keycode1 (+ mod2 mod1) #f proc-release)
    ;; now handle keycode2 last
    (bind-keycode 'all keycode2 mod1 proc-press #f)
    (bind-keycode 'all keycode2 (+ mod1 mod2) #f proc-release)))


(define (bind-three-modifier-key-events modkey1 modkey2 modkey3
					proc-press proc-release)
  (let ((keycode1 (car modkey1))
	(mod1 (cdr modkey1))
	(keycode2 (car modkey2))
	(mod2 (cdr modkey2))
	(keycode3 (car modkey3))
	(mod3 (cdr modkey3)))
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
      )))

(define (car-or-255 l)
  (if (and (list? l) (not (eq? l '()))) (car l) 255))

(begin
  (define XKM_CONTROL_L (cons (car-or-255 (keysym->keycode "Control_L")) (mod-mask-control)))
  (define XKM_META_L (cons (car-or-255 (keysym->keycode "Meta_L")) (mod-mask-meta)))
  (define XKM_ALT_L (cons (car-or-255 (keysym->keycode "Alt_L")) (mod-mask-alt)))
  (define XKM_SHIFT_L (cons (car-or-255 (keysym->keycode "Shift_L")) (mod-mask-shift)))
  (define XKM_HYPER_L (cons (car-or-255 (keysym->keycode "Hyper_L")) (mod-mask-hyper)))
  (define XKM_SUPER_L (cons (car-or-255 (keysym->keycode "Super_L")) (mod-mask-super)))
  )

(bind-two-modifier-key-events 
 XKM_CONTROL_L  XKM_META_L
;; (37 . 4) (64 . 8)
 (lambda () (move-window 5 5 (current-window-with-focus)))
 (lambda () (move-window 10 10 (current-window-with-focus))))


(bind-three-modifier-key-events 
 XKM_CONTROL_L  XKM_ALT_L  XKM_SHIFT_L
;; (37 . 4) (115 . 16) (50 . 1)
 (lambda () (move-window 30 30 (current-window-with-focus)))
 (lambda () (move-window 50 50 (current-window-with-focus))))

;; Highlight the window about to be effected by the mouse move/resize
(bind-three-modifier-key-events 
 XKM_CONTROL_L  XKM_SHIFT_L  XKM_META_L
 (lambda () (set-window-highlight-background! "red" (current-window-with-pointer)))
 (lambda () (set-window-highlight-background! "navyblue" (current-window-with-pointer))))
