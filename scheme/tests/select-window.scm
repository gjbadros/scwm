;; $Id$
;; --07/25/98 gjb

(map (lambda (s) (select-window-interactively s))
     (list "A win"
	   "Another win"
	   "A really really really long window name"
	   "A really really really long window name that is even longer because it just contains junk lots and lots of junk and lots of junk junk junk "))
