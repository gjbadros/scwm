;; $Id$


(use-modules (app scwm scwmproplist))


(define pl (string->proplist "foo"))


(proplist-get-string-description pl)


(define pll (map string->proplist (list "greg" "dude" "toby" "bloke")))


(define pld (apply proplist-make-dictionary-from-entries pll))

(proplist-get-array-element (proplist-get-all-dictionary-keys pld) 1)

(proplist-get-dictionary-entry pld (string->proplist "greg"))

(proplist-set-filename pld  "greg.txt")
(proplist-save pld #t)
(proplist-get-array-element (proplist-get-all-dictionary-keys pld) 1)
;; now edit the file (I have to eval the below twice... why? GJB:FIXME::
(proplist-synchronize pld)
(proplist-get-array-element (proplist-get-all-dictionary-keys pld) 1)



(define p (get-proplist-with-path "WindowMaker"))

(define p-keys (proplist-get-all-dictionary-keys p))

(proplist-get-number-of-elements p2)

(proplist-get-array-element p2 0)

(proplist-get-dictionary-entry p "NoDithering")

(map (lambda (i) (proplist-get-array-element p-keys i))
     (list 0 1 2 3))

(proplist-get-all-dictionary-keys p)
