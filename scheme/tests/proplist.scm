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


(chdir "/home/gjb/scwm/scheme/tests")
(define p (get-proplist-with-path "/home/gjb/GNUstep/Defaults/WindowMaker"))

(proplist-synchronize p)  ;; do this after saves -- use simplebiff?

(define p-keys (proplist-get-all-dictionary-keys p))

(proplist-get-number-of-elements p-keys)

(proplist-get-array-element p-keys 0)

(proplist-get-dictionary-entry p "NoDithering")
(proplist-get-dictionary-entry p "WindowPlacement")

(map (lambda (i) (proplist-get-array-element p-keys i))
     (list 0 1 2 3))

(proplist-get-all-dictionary-keys p)

(define* (proplist-dictionary->alist pl #:optional (i 0))
  (let* ((keys (proplist-get-all-dictionary-keys pl))
	 (num (proplist-get-number-of-elements keys)))
    (if (>= i num)
	'()
	(begin
	  (let* ((key (proplist-get-array-element keys i))
		 (val (proplist-get-dictionary-entry pl key)))
	    (cons (cons key val) (proplist-dictionary->alist pl (+ i 1))))))))

(define al (proplist-dictionary->alist p))

(assoc-ref al "ShadeDelay")
