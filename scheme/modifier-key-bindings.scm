;; $Id$
;; Copyright (C) 1999 Greg J. Badros
;; GJB:FIXME:: need to add unbind-N-modifer-key-events procedures
;; Should do it nicely w/o duplicating all code

(define-module (app scwm modifier-key-bindings)
  :use-module (app scwm optargs))

(define (do-two-modifier-key-events proc
				    modkey1 modkey2 
				    proc-press proc-release)
  (let ((keycode1 (car modkey1))
	(mod1 (cdr modkey1))
	(keycode2 (car modkey2))
	(mod2 (cdr modkey2)))
    ;; first handle keycode1 last 
    (proc 'all keycode1 mod2 proc-press #f)
    (proc 'all keycode1 (+ mod2 mod1) #f proc-release)
    ;; now handle keycode2 last
    (proc 'all keycode2 mod1 proc-press #f)
    (proc 'all keycode2 (+ mod1 mod2) #f proc-release)))

(define-public (bind-two-modifier-key-events
		modkey1 modkey2 
		proc-press proc-release)
  "Bind PROC-PRESS and PROC-RELEASE to be invoked on a multi-modifier key event.
MODKEY1 and MODKEY2 are the two modifiers that, when pressed at the same time,
will invoke PROC-PRESS.  When either is released, PROC-RELEASE is invoked."
  (do-two-modifier-key-events bind-keycode modkey1 modkey2 
			      proc-press proc-release))


(define-public (unbind-two-modifier-key-events
		modkey1 modkey2)
  "Unbind events attached to a multi-modifier key event."
  (do-two-modifier-key-events unbind-keycode modkey1 modkey2 
			      #f #f))



(define (do-three-modifier-key-events proc
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
      (proc 'all keycode1 (+ mod2 mod3) proc-press #f)
      (proc 'all keycode1 all-mods #f proc-release)
      ;; now handle keycode2 last
      (proc 'all keycode2 (+ mod1 mod3) proc-press #f)
      (proc 'all keycode2 all-mods #f proc-release)
      ;; now handle keycode 3 last
      (proc 'all keycode3 (+ mod1 mod2) proc-press #f)
      (proc 'all keycode3 all-mods #f proc-release)
      )))

(define-public (bind-three-modifier-key-events 
		modkey1 modkey2 modkey3
		proc-press proc-release)
  "Bind PROC-PRESS and PROC-RELEASE to be invoked on a multi-modifier key event.
MODKEY1, MODKEY2, MODKEY3 are the three modifiers that, when pressed at the same time,
will invoke PROC-PRESS.  When either is released, PROC-RELEASE is invoked."
  (do-three-modifier-key-events bind-keycode modkey1 modkey2 modkey3
				proc-press proc-release))

(define*-public (unbind-three-modifier-key-events
		modkey1 modkey2 modkey3 #&optional ignore1 ignore2)
  "Unbind events attached to a multi-modifier key event.
MODKEY1, MODKEY2, MODKEY3 are the three modifiers that are having the binding
eliminated for.  IGNORE1 and IGNORE2 can be anything, and are
provided to conveniently allow an unbinding invocation to 
have the same arguments as the analagous binding operation."
  (do-three-modifier-key-events unbind-keycode modkey1 modkey2 modkey3
				#f #f))


(define (do-four-modifier-key-events proc
				     modkey1 modkey2 modkey3 modkey4
				     proc-press proc-release)
  (let ((keycode1 (car modkey1))
	(mod1 (cdr modkey1))
	(keycode2 (car modkey2))
	(mod2 (cdr modkey2))
	(keycode3 (car modkey3))
	(mod3 (cdr modkey3))
	(keycode4 (car modkey4))
	(mod4 (cdr modkey4)))
    (let ((all-mods (+ mod1 mod2 mod3 mod4)))
      ;; first handle keycode1 last
      (proc 'all keycode1 (+ mod2 mod3 mod4) proc-press #f)
      (proc 'all keycode1 all-mods #f proc-release)
      ;; now handle keycode2 last
      (proc 'all keycode2 (+ mod1 mod3 mod4) proc-press #f)
      (proc 'all keycode2 all-mods #f proc-release)
      ;; now handle keycode 3 last
      (proc 'all keycode3 (+ mod1 mod2 mod4) proc-press #f)
      (proc 'all keycode3 all-mods #f proc-release)
      ;; now handle keycode 4 last
      (proc 'all keycode4 (+ mod1 mod2 mod3) proc-press #f)
      (proc 'all keycode4 all-mods #f proc-release)
      )))

(define-public (bind-four-modifier-key-events 
		modkey1 modkey2 modkey3 modkey4
		proc-press proc-release)
  "Bind PROC-PRESS and PROC-RELEASE to be invoked on a multi-modifier key event.
MODKEY1, MODKEY2, MODKEY3, MODKEY4 are the four modifiers that, when pressed at the same time,
will invoke PROC-PRESS.  When either is released, PROC-RELEASE is invoked."
  (do-four-modifier-key-events bind-keycode modkey1 modkey2 modkey3 modkey4
			       proc-press proc-release))

(define*-public (unbind-four-modifier-key-events
		modkey1 modkey2 modkey3 modkey4 #&optional ignore1 ignore2 )
  "Unbind events attached to a multi-modifier key event.
MODKEY1, MODKEY2, MODKEY3, MODKEY4 are the four modifiers that are having the binding
eliminated for.  IGNORE1 and IGNORE2 can be anything, and are
provided to conveniently allow an unbinding invocation to 
have the same arguments as the analagous binding operation."
  (do-four-modifier-key-events unbind-keycode modkey1 modkey2 modkey3 modkey4
			       #f #f))



(define (car-or-255 l)
  (if (and (list? l) (not (eq? l '()))) (car l) 255))

(define (key-keycode-and-modifier key modifier)
  (if modifier
      (cons (car-or-255 (keysym->keycode key)) modifier)
      #f))

(define-public XKM_CONTROL_L 
  (key-keycode-and-modifier "Control_L" (mod-mask-control)))
(define-public XKM_META_L
  (key-keycode-and-modifier "Meta_L" (mod-mask-meta)))
(define-public XKM_ALT_L
  (key-keycode-and-modifier "Alt_L" (mod-mask-alt)))
(define-public XKM_SHIFT_L
  (key-keycode-and-modifier "Shift_L" (mod-mask-shift)))
(define-public XKM_HYPER_L
  (key-keycode-and-modifier "Hyper_L" (mod-mask-hyper)))
(define-public XKM_SUPER_L
  (key-keycode-and-modifier "Super_L" (mod-mask-super)))

(define (mod-mask-tester?? mm)
  (lambda (modmask)
    (if (= (logand mm modmask) 0) #f #t)))

(define-public mod-mask-shift? (mod-mask-tester?? (mod-mask-shift)))
(define-public mod-mask-control? (mod-mask-tester?? (mod-mask-control)))
(define-public mod-mask-meta? (mod-mask-tester?? (mod-mask-meta)))
(define-public mod-mask-hyper? (mod-mask-tester?? (mod-mask-hyper)))
(define-public mod-mask-alt? (mod-mask-tester?? (mod-mask-alt)))
(define-public mod-mask-super? (mod-mask-tester?? (mod-mask-super)))

