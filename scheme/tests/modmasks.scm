;; these return a power of 2 or 0 if the modifier isn't bound
(mod-mask-meta)
(mod-mask-alt)
(mod-mask-hyper)
(mod-mask-super)

(apropos-internal "mod-")
;; my keycode 96 is F12
;; xmodmap -e 'keycode 96 = Super_R'
;; 

(reset-hook! X-MappingNotify-hook)

(add-hook! X-MappingNotify-hook (lambda () (display "new mods\n")))
