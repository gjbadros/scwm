

(defmacro @ args
  `(lambda (sym)
     (variable-ref (module-variable (resolve-module ',args) sym))))

(define (user-var sym) (variable-ref (module-variable the-root-module sym)))

(defmacro user-var (sym) 
  `(variable-ref (module-variable the-root-module ',sym)))


(define $ (@ app scwm user-variables))

the-root-module

(define-module (app scwm user-variables))  


(module-uses the-root-module)

(set-current-module the-root-module)
(define bing 2)
(define baz 3)

(set-current-module (resolve-module '(app scwm user-variables)))
(define foo 4)
(define-public bar 6)


foo
bar

($ 'foo)
($ 'bar)
($ 'bing)
($ 'baz)

(define foo 6)


opaque-move-percent
(move-opaquely? (current-window-with-pointer))

((@ app scwm winops) 'opaque-move-percent)

(user-var opaque-move-percent)
