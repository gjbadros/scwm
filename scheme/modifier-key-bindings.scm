;; $Id$
;; Copyright (C) 1999 Greg J. Badros

(define-module (app scwm modifier-key-bindings))

(define-public (bind-two-modifier-key-events 
		modkey1 modkey2 
		proc-press proc-release)
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


(define-public (bind-three-modifier-key-events 
		modkey1 modkey2 modkey3
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

(define-public XKM_CONTROL_L
  (cons (car-or-255 (keysym->keycode "Control_L")) (mod-mask-control)))
(define-public XKM_META_L
  (cons (car-or-255 (keysym->keycode "Meta_L")) (mod-mask-meta)))
(define-public XKM_ALT_L
  (cons (car-or-255 (keysym->keycode "Alt_L")) (mod-mask-alt)))
(define-public XKM_SHIFT_L
  (cons (car-or-255 (keysym->keycode "Shift_L")) (mod-mask-shift)))
(define-public XKM_HYPER_L
  (cons (car-or-255 (keysym->keycode "Hyper_L")) (mod-mask-hyper)))
(define-public XKM_SUPER_L
  (cons (car-or-255 (keysym->keycode "Super_L")) (mod-mask-super)))
